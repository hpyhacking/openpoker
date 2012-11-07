-module(player).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, start_link/1, stop/1, notify/2, cast/2, auth/2, logout/1]).

-export([client/1, info/1, balance/1]).

-export([ctx/1, ctx/2]).

-include("genesis.hrl").

-record(pd, { %% process data
    pid,
    self,
    identity = ?UNDEF,
    client = ?UNDEF,
    playing = ?UNDEF,
    playing_sn = 0,
    watching = ?UNDEF,
    nick = ?UNDEF,
    photo = ?UNDEF,
    inplay = 0,
    record
  }).

init([R = #tab_player_info{pid = PId, identity = Identity, nick = Nick, photo = Photo}]) ->
  process_flag(trap_exit, true),
  ok = create_runtime(PId, self()),
  {ok, #pd{pid = PId, self = self(), nick = list_to_binary(Nick), photo = list_to_binary(Photo), identity = Identity, record = R}}.

handle_cast(R = #cmd_watch{game = G}, Data = #pd{})
when Data#pd.watching =:= ?UNDEF ->
  game:watch(G, R#cmd_watch{pid = Data#pd.pid, identity = Data#pd.identity}),
  {noreply, Data};

handle_cast(R = #cmd_unwatch{game = G}, Data = #pd{})
when Data#pd.watching /= ?UNDEF ->
  game:unwatch(G, R#cmd_unwatch{pid = Data#pd.pid, identity = Data#pd.identity}),
  {noreply, Data};

handle_cast(#cmd_join{game = G, buyin = B}, Data = #pd{watching = W, playing = P, record = R}) when is_pid(G), W =:= G, P =:= ?UNDEF, (R#tab_player_info.cash + R#tab_player_info.credit) < B  ->
  notify(#notify_error{error = ?ERR_JOIN_LESS_BALANCE}),
  {noreply, Data};

handle_cast(R = #cmd_join{game = G}, Data = #pd{watching = W, playing = P}) when is_pid(G), W =:= G, P =:= ?UNDEF ->
  game:join(G, R#cmd_join{pid = Data#pd.pid, identity = Data#pd.identity, nick = Data#pd.nick, photo = Data#pd.photo}),
  {noreply, Data};

handle_cast(R = #cmd_join{game = G}, Data = #pd{identity = Identity, watching = W, playing = P}) when is_pid(G), W =:= ?UNDEF, P =:= ?UNDEF ->
  game:watch(G, Identity),
  handle_cast(R, Data#pd{watching = G});

handle_cast(R = #cmd_join{}, Data = #pd{}) ->
  ?LOG([{player, {error, join}}, {join, R}, {pd, Data}]),
  {noreply, Data};

%% player leave game
handle_cast(R = #cmd_leave{game = G}, Data = #pd{playing = P, playing_sn = SN}) when G =:= P, SN /= 0 ->
  game:leave(G, R#cmd_leave{pid = Data#pd.pid, sn = SN}),
  {noreply, Data};

handle_cast(#cmd_leave{}, Data) ->
  {noreply, Data};

%% player info query
handle_cast(#cmd_query_player{}, Data = #pd{}) ->
  R = #notify_player{
    player = Data#pd.pid,
    nick = Data#pd.nick,
    photo = Data#pd.photo
  },
  handle_cast({notify, R}, Data);

handle_cast(#cmd_query_balance{}, Data) ->
  R = #notify_acount{ balance = 0, inplay = 0 },
  handle_cast({notify, R}, Data);

handle_cast(#cmd_query_seats{game = Game}, Data) ->
  game:query_seats(Game),
  {noreply, Data};

handle_cast({notify, R = #notify_watch{proc = G, player = P}}, Data = #pd{pid = PId})
when P =:= PId ->
  forward_to_client(R, Data),
  {noreply, Data#pd{ watching = G }};

handle_cast({notify, R = #notify_unwatch{player = P}}, Data = #pd{pid = PId})
when P =:= PId ->
  forward_to_client(R, Data),
  {noreply, Data#pd{ watching = ?UNDEF }};

handle_cast({notify, R = #notify_join{proc = G, player = P}}, Data = #pd{pid = PId}) when P =:= PId ->
  Info = reload_player_info(PId),
  forward_to_client(R, Data),
  {noreply, Data#pd{ playing = G, playing_sn = R#notify_join.sn, record = Info }};

handle_cast({notify, R = #notify_leave{player = P}}, Data = #pd{pid = PId}) when P =:= PId ->
  Info = reload_player_info(PId),
  forward_to_client(R, Data),
  {noreply, Data#pd{ playing = ?UNDEF, playing_sn = 0, record = Info }};

handle_cast({notify, R}, Data) ->
  forward_to_client(R, Data),
  {noreply, Data};

handle_cast(stop, Data) ->
  {stop, normal, Data};

handle_cast({stop, Reason}, Data) ->
  {stop, Reason, Data};

handle_cast(R = #cmd_raise{game = G}, Data = #pd{playing = P}) when G =:= P ->
  game:raise(G, R#cmd_raise{sn = Data#pd.playing_sn, pid = Data#pd.pid}),
  {noreply, Data};

handle_cast(R = #cmd_fold{game = G}, Data = #pd{playing = P}) when G =:= P ->
  game:fold(G, R#cmd_fold{pid = Data#pd.pid}),
  {noreply, Data};

handle_cast(logout, Data = #pd{playing = ?UNDEF}) ->
  error_logger:info_report({player_loggout, nothing_join_game}),
  {noreply, Data};

handle_cast(logout, Data = #pd{playing = Game}) ->
  error_logger:info_report({player_loggout, join_game, Game}),
  {noreply, handle_cast(#cmd_leave{game = Game}, Data)};
  
handle_cast(R, Data) ->
  ?LOG([{unknown_cast, R}]),
  {noreply, Data}.

handle_call(ctx, _From, Data) ->
  {reply, Data, Data};

handle_call(info, _From, Data = #pd{record = R}) ->
  {reply, R, Data};

handle_call({client, Client}, _From, Data) when is_pid(Client) ->
  {reply, Client, Data#pd{ client = Client}};

handle_call(kill, _From, Data) ->
  {stop, normal, ok, Data};

handle_call(plist, _From, Data) ->
  {reply, [{nick, Data#pd.nick}, {photo, Data#pd.photo}, {pid, Data#pd.pid}, {proc, Data#pd.self}], Data};

handle_call(R, From, Data) ->
  error_logger:info_report([{module, ?MODULE}, {process, self()}, {unknown_call, R}, {from, From}]),
  {noreply, Data}.

terminate(_Reason, Data) ->
  ok = mnesia:dirty_delete(tab_player, Data#pd.pid).

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(_Info, Data) ->
  {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
  {ok, Data}.

%%%
%%% clinet function
%%% 

ctx(PId) ->
  ctx(PId, ctx).

ctx(PId, Type) ->
  gen_server:call(?LOOKUP_PLAYER(PId), Type).

start_link(R = #tab_player_info{pid = PId}) ->
  gen_server:start_link(?PLAYER(PId), ?MODULE, [R], []).

start(R = #tab_player_info{pid = PId}) ->
  gen_server:start(?PLAYER(PId), ?MODULE, [R], []).

stop(PId) when is_integer(PId) ->
  gen_server:cast(?PLAYER(PId), stop);

stop(Pid) when is_pid(Pid) ->
  gen_server:cast(Pid, stop).

notify(R) ->
  notify(self(), R).

notify(PId, R) when is_integer(PId) ->
  notify(?LOOKUP_PLAYER(PId), R);
notify(Player, R) when is_pid(Player) ->
  gen_server:cast(Player, {notify, R}).

cast(Player, R) when is_pid(Player) ->
  gen_server:cast(Player, R).

auth(Identity, Password) when is_list(Identity), is_list(Password) ->
  ok = mnesia:wait_for_tables([tab_player_info], ?WAIT_TABLE),
  case mnesia:dirty_index_read(tab_player_info, Identity, identity) of
    [Info] ->
      auth(Info, Password);
    _ ->
      {ok, unauth}
  end;

auth(Info = #tab_player_info{password = Pwd}, Password) when is_list(Password) ->
  %% password processed by phash, result is a integer
  case erlang:phash2(Password, 1 bsl 32) =:= Pwd of
    true -> auth(Info, player_disable);
    _ -> {ok, unauth}
  end;

auth(Info = #tab_player_info{disabled = Disabled}, player_disable) ->
  case Disabled of
    false -> {ok, pass, Info};
    _ -> {ok, player_disable}
  end.

logout(Player) when is_pid(Player) ->
  gen_server:cast(Player, logout).

info(Player) when is_pid(Player) ->
  gen_server:cast(Player, #cmd_query_player{}).

balance(Player) when is_pid(Player) ->
  gen_server:cast(Player, #cmd_query_balance{}).

client(Player) when is_pid(Player) ->
  Client = self(),
  Client = gen_server:call(Player, {client, Client}).

%%%
%%% private
%%%

reload_player_info(PId) ->
  {atomic, R} = mnesia:transaction(fun() ->
        [Info] = mnesia:read(tab_player_info, PId),
        Info
    end),
  R.

create_runtime(PID, Process) when is_number(PID), is_pid(Process) ->
  mnesia:dirty_write(#tab_player{ pid = PID, process = Process }).

forward_to_client(_, #pd{client = Client}) when Client =:= ?UNDEF -> 
  exit(undefined_client);
forward_to_client(R, #pd{client = Client}) -> 
  genesis_game_handler:send(Client, R).
