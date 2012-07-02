-module(game).
-behaviour(exch).

-export([id/0, init/2, stop/1, dispatch/2, call/2]).
-export([start/0, start/1, start_conf/2]).

-export([ctx/1, state/1]).

-export([watch/2, unwatch/2, join/2, leave/2, bet/2, reward/3, query_seats/1, info/1, list/0]).
-export([raise/2, fold/2]).
-export([broadcast/2, broadcast/3]).

-include("genesis.hrl").

%%%
%%% callback
%%% 

id() ->
  counter:bump(game).

init(GID, R = #tab_game_config{}) ->
  create_runtime(GID, R),
  #texas { 
    gid = GID,
    seats = seat:new(R#tab_game_config.seat_count),
    max_joined = R#tab_game_config.seat_count,
    limit = R#tab_game_config.limit,
    timeout = ?PLAYER_TIMEOUT,
    start_delay = R#tab_game_config.start_delay,
    required = R#tab_game_config.required,
    xref = gb_trees:empty(),
    pot = pot:new(),
    deck = deck:new(),
    b = ?UNDEF,
    sb = ?UNDEF,
    bb = ?UNDEF
  }.

stop(#texas{gid = GID, timer = Timer}) ->
  catch erlang:cancel_timer(Timer),
  clear_runtime(GID).

call({watch, {Identity, Process}}, Ctx = #texas{observers = Obs}) ->
  R = #notify_game_detail{
    game = Ctx#texas.gid, 
    pot = pot:total(Ctx#texas.pot),
    stage = Ctx#texas.stage,
    limit = Ctx#texas.limit,
    seats = seat:info(size, Ctx#texas.seats),
    require = Ctx#texas.required,
    joined = Ctx#texas.joined
  },

  player:notify(Process, R),

  %% update observer player process
  case proplists:lookup(Identity, Obs) of
    none ->
      {ok, ok, Ctx#texas{observers = [{Identity, Process}|Obs]}};
    {Identity, Process} ->
      NewObs = [{Identity, Process}] ++ proplists:delete(Identity, Obs),
      {ok, ok, Ctx#texas{observers = NewObs}}
  end;

call({unwatch, Identity}, Ctx = #texas{observers = Obs}) ->
  case proplists:lookup(Identity, Obs) of
    none ->
      {ok, ok, Ctx};
    {Identity, _Proc} ->
      {ok, ok, Ctx#texas{observers = proplists:delete(Identity, Obs)}}
  end;

call(info, Ctx = #texas{gid = GId, joined = Joined, required = Required, seats = Seats, limit = Limit}) ->
  {ok, #notify_game{
      game = GId,
      name = <<"TEXAS_TABLE">>,
      limit = Limit,
      seats = seat:info(size, Seats),
      require = Required,
      joined = Joined
    }, Ctx};

call(pdata, Ctx) ->
  {ok, Ctx, Ctx}.

dispatch(#cmd_join{buyin = Buyin}, Ctx = #texas{limit = Limit, joined = Joined, max_joined = MaxJoin})
when Joined =:= MaxJoin; Buyin < Limit#limit.min; Buyin > Limit#limit.max ->
  Ctx;

dispatch(R = #cmd_join{sn = SN}, Ctx = #texas{seats = Seats}) when SN /= 0 ->
  case seat:get(SN, Seats) of
    Seat = #seat{state = ?PS_EMPTY} ->
      do_join(R, Seat, Ctx);
    _ ->
      dispatch(R#cmd_join{sn = 0}, Ctx)
  end;

dispatch(R = #cmd_join{sn = SN}, Ctx = #texas{seats = Seats}) when SN =:= 0 ->
  %% auto compute player seat number
  [H = #seat{}|_] = seat:lookup(?PS_EMPTY, Seats),
  do_join(R, H, Ctx);
  
dispatch(R = #cmd_leave{sn = SN, pid = PId}, Ctx = #texas{exp_seat = Exp, seats = Seats}) ->
  case seat:get(SN, Seats) of
    #seat{pid = PId} ->
      Fun = fun() -> 
          [Info] = mnesia:read(tab_player_info, PId, write),
          [Inplay] = mnesia:read(tab_inplay, PId, write),

          case Inplay#tab_inplay.inplay < 0 of
            true ->
              exit(err_inplay_less_zero);
            _ ->
              ok
          end,

          ok = mnesia:delete_object(Inplay),
          ok = mnesia:write(#tab_buyin_log{
              pid = R#cmd_leave.pid, gid = Ctx#texas.gid, 
              amt = Inplay#tab_inplay.inplay, cash = Info#tab_player_info.cash + Inplay#tab_inplay.inplay,
              credit = Info#tab_player_info.credit}),
          ok = mnesia:write(Info#tab_player_info{cash = Info#tab_player_info.cash + Inplay#tab_inplay.inplay})
      end,

      case mnesia:transaction(Fun) of
        {atomic, ok} ->
          LeaveMsg = #notify_leave{
            game = Ctx#texas.gid,
            sn = SN, player = PId,
            proc = self()
          },

          LeavedExp = case Exp of
            #seat{pid = P} when P =:= PId ->
              game:fold(self(), #cmd_fold{pid = PId}),
              Exp#seat{state = ?PS_LEAVE};
            _ ->
              Exp
          end,

          broadcast(LeaveMsg, Ctx),
          LeavedSeats = seat:set(SN, ?PS_LEAVE, Seats),
          Ctx#texas{seats = LeavedSeats, joined = Ctx#texas.joined - 1, exp_seat = LeavedExp};
        {aborted, Err} ->
          ?LOG([{game, error}, {leave, R}, {ctx, Ctx}, {error, Err}]),
          Ctx
      end;
    _ ->
      ?LOG([{game, error}, {leave, R}, {ctx, Ctx}, {error, not_find_player}]),
      Ctx
  end;

dispatch({query_seats, Player}, Ctx) when is_pid(Player)->
  Fun = fun(R) ->
      R1 = #notify_seat{
        game = Ctx#texas.gid,
        sn = R#seat.sn,
        state = R#seat.state,
        player = R#seat.pid,
        inplay = R#seat.inplay,
        bet = R#seat.bet,
        nick = R#seat.nick,
        photo = R#seat.photo
      },

      player:notify(Player, R1)
  end,
  lists:map(Fun, seat:get(Ctx#texas.seats)),
  Ctx;

dispatch(_, Ctx) ->
  Ctx.

%%%
%%% client
%%%

start() ->
  Fun = fun(R = #tab_game_config{max = Max}, _Acc) ->
      start_conf(R, Max)
  end, 

  ok = mnesia:wait_for_tables([tab_game_config], ?WAIT_TABLE),
  {atomic, Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, nil, tab_game_config) end),
  Result.

start(Mods) when is_list(Mods)->
  Conf = #tab_game_config{id = 1, module = game, mods = Mods, limit = no_limit, seat_count = 9, start_delay = 3000, required = 2, timeout = 1000, max = 1},
  start_conf(Conf, 1);

start(Conf = #tab_game_config{max = Max}) ->
  start_conf(Conf, Max).

start_conf(Conf, N) -> 
  start_conf(Conf, N, []).

start_conf(_Conf, 0, L) -> L;
start_conf(Conf = #tab_game_config{module = Module, mods = Mods}, N, L) when is_list(Mods) ->
  {ok, Pid} = exch:start(Module, Conf, Mods),
  start_conf(Conf, N - 1, L ++ [{ok, Pid}]);
start_conf(Conf = #tab_game_config{}, N, L) ->
  start_conf(Conf#tab_game_config{mods = default_mods()}, N, L).

%% check
bet({R = #seat{}, Amt}, Ctx = #texas{}) ->
  bet({R, Amt, 0}, Ctx);

bet({R = #seat{sn = SN}, _Call = 0, _Raise = 0}, Ctx = #texas{seats = Seats}) ->
  broadcast(#notify_raise{game = Ctx#texas.gid, player = R#seat.pid, raise = 0, call = 0}, Ctx),
  NewSeats = seat:set(SN, ?PS_BET, Seats),
  Ctx#texas{seats = NewSeats};

%% call & raise
bet({S = #seat{inplay = Inplay, bet = Bet, pid = PId}, Call, Raise}, Ctx = #texas{seats = Seats}) ->
  Amt = Call + Raise,
  {State, AllIn, CostAmt} = case Amt < Inplay of 
    true -> {?PS_BET, false, Amt}; 
    _ -> {?PS_ALL_IN, true, Inplay} 
  end,

  Fun = fun() ->
      [R] = mnesia:read(tab_inplay, PId, write),
      ok = mnesia:write(R#tab_inplay{inplay = Inplay - CostAmt}),
      ok = mnesia:write(#tab_turnover_log{
          pid = PId, game = Ctx#texas.gid,
          amt = 0 - CostAmt, cost = 0, inplay = Inplay - CostAmt})
  end,
  
  case mnesia:transaction(Fun) of
    {atomic, ok} ->
      broadcast(#notify_raise{game = Ctx#texas.gid, player = PId, raise = Raise, call = Call}, Ctx),
      NewSeats = seat:set(S#seat{inplay = Inplay - CostAmt, state = State, bet = Bet + CostAmt}, Seats),
      NewPot = pot:add(Ctx#texas.pot, PId, Amt, AllIn),
      Ctx#texas{seats = NewSeats, pot = NewPot}
  end.

reward(#hand{seat_sn = SN, pid = PId}, Amt, Ctx = #texas{seats = Seats}) when Amt > 0 ->
  WinAmt = Amt,
  Seat = seat:get(SN, Seats),
  PId = Seat#seat.pid,

  Fun = fun() ->
      [R] = mnesia:read(tab_inplay, PId, write),
      RewardInplay = R#tab_inplay.inplay + WinAmt,
      ok = mnesia:write(R#tab_inplay{inplay = R#tab_inplay.inplay + WinAmt}),
      ok = mnesia:write(#tab_turnover_log{
          pid = Seat#seat.pid, game = Ctx#texas.gid,
          amt = WinAmt, cost = Amt - WinAmt, inplay = RewardInplay}),
      RewardInplay
  end,
  
  case mnesia:transaction(Fun) of
    {atomic, RewardInplay} ->
      broadcast(#notify_win{ game = Ctx#texas.gid, player = PId, amount = WinAmt}, Ctx),
      RewardedSeats = seat:set(Seat#seat{inplay = RewardInplay, bet = 0}, Seats),
      Ctx#texas{seats = RewardedSeats}
  end.

broadcast(Msg, #texas{observers = Obs}, []) ->
  broadcast(Msg, Obs);
broadcast(Msg, Ctx = #texas{observers = Obs}, _Exclude = [H|T]) ->
  broadcast(Msg, Ctx#texas{observers = proplists:delete(H, Obs)}, T).

broadcast(Msg, #texas{observers = Obs}) -> 
  broadcast(Msg, Obs);
broadcast(_Msg, []) -> ok;
broadcast(Msg, [{_, Process}|T]) ->
  player:notify(Process, Msg),
  broadcast(Msg, T).

info(GId) when is_integer(GId) ->
  gen_server:call(?LOOKUP_GAME(GId), info);
info(Game) when is_pid(Game) ->
  gen_server:call(Game, info).

list() ->
  Fun = fun(#tab_game_xref{process = Game}, Acc) ->
      Acc ++ [info(Game)]
  end,
  {atomic, Result} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], tab_game_xref) end),
  Result.

watch(Game, Identity) when is_pid(Game), is_list(Identity) ->
  gen_server:call(Game, {watch, {Identity, self()}}).

unwatch(Game, Identity) when is_pid(Game), is_list(Identity) ->
  gen_server:call(Game, {unwatch, Identity}).

join(Game, R = #cmd_join{}) when is_pid(Game) ->
  gen_server:cast(Game, R#cmd_join{proc = self()}).

leave(Game, R = #cmd_leave{}) when is_pid(Game) ->
  gen_server:cast(Game, R).

raise(Game, R = #cmd_raise{}) when is_pid(Game) ->
  gen_server:cast(Game, R).

fold(Game, R = #cmd_fold{}) when is_pid(Game) ->
  gen_server:cast(Game, R).

query_seats(Game) when is_pid(Game) ->
  gen_server:cast(Game, {query_seats, self()}).

ctx(Id) ->  % get exch context
  gen_server:call(?LOOKUP_GAME(Id), ctx).

state(Id) -> % get exch pdata
  gen_server:call(?LOOKUP_GAME(Id), {pdata, state}).

%%%
%%% private
%%%

create_runtime(GID, R) ->
  mnesia:dirty_write(#tab_game_xref {
      gid = GID,
      process = self(),
      module = R#tab_game_config.module,
      limit = R#tab_game_config.limit,
      seat_count = R#tab_game_config.seat_count,
      timeout = R#tab_game_config.timeout,
      required = R#tab_game_config.required
  }).

clear_runtime(GID) ->
  ok = mnesia:dirty_delete(tab_game_xref, GID).

default_mods() ->
  [
    {wait_players, []},
    %% blind rules
    {blinds, []},
    %% deal 2 cards to each player
    {deal_cards, [2, private]}, 
    {ranking, []}, 
    %% start after BB, 3 raises
    {betting, [?GS_PREFLOP]}, 
    {deal_cards, [3, shared]}, 
    {ranking, []}, 
    %% flop
    {betting, [?GS_FLOP]}, 
    {deal_cards, [1, shared]}, 
    {ranking, []}, 
    %% turn
    {betting, [?GS_TURN]}, 
    {deal_cards, [1, shared]}, 
    {ranking, []}, 
    %% river
    {betting, [?GS_RIVER]}, 
    {showdown, []},
    {restart, []}
  ].

do_join(R = #cmd_join{identity = Identity, proc = Process}, Seat = #seat{}, Ctx = #texas{observers = Obs, seats = Seats}) ->
  case proplists:lookup(Identity, Obs) of
    {Identity, Process} ->
      JoinedSeats = seat:set(Seat#seat{
          identity = Identity,
          pid = R#cmd_join.pid,
          process = Process,
          hand = hand:new(),
          bet = 0,
          inplay = R#cmd_join.buyin,
          state = ?PS_WAIT,
          nick = R#cmd_join.nick,
          photo = R#cmd_join.photo
        }, Seats),


      JoinMsg = #notify_join{
        game = Ctx#texas.gid,
        player = R#cmd_join.pid,
        sn = Seat#seat.sn,
        buyin = R#cmd_join.buyin,
        nick = R#cmd_join.nick,
        photo = R#cmd_join.photo,
        proc = self()
      },

      Fun = fun() ->
          [] = mnesia:read(tab_inplay, R#cmd_join.pid), % check none inplay record
          [Info] = mnesia:read(tab_player_info, R#cmd_join.pid, write),
          Balance = Info#tab_player_info.cash + Info#tab_player_info.credit,

          case Balance < R#cmd_join.buyin of
            true ->
              exit(err_join_less_balance);
            _ ->
              ok
          end,

          ok = mnesia:write(#tab_buyin_log{
              pid = R#cmd_join.pid, gid = Ctx#texas.gid, 
              amt = 0 - R#cmd_join.buyin, cash = Info#tab_player_info.cash - R#cmd_join.buyin,
              credit = Info#tab_player_info.credit}),
          ok = mnesia:write(#tab_inplay{pid = R#cmd_join.pid, inplay = R#cmd_join.buyin}),
          ok = mnesia:write(Info#tab_player_info{cash = Info#tab_player_info.cash - R#cmd_join.buyin})
      end,

      case mnesia:transaction(Fun) of
        {atomic, ok} ->
          broadcast(JoinMsg, Ctx),
          Ctx#texas{seats = JoinedSeats, joined = Ctx#texas.joined + 1};
        {aborted, Err} ->
          ?LOG([{game, error}, {join, R}, {ctx, Ctx}, {error, Err}]),
          Ctx
      end;
    _ -> % not find watch in observers
      ?LOG([{game, error}, {join, R}, {ctx, Ctx}, {error, not_find_observer}]),
      Ctx
  end.
