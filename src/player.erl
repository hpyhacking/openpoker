-module(player).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1, stop/2, notify/2, cast/2, auth/2, logout/1]).

-export([client/1, info/1, balance/1]).

-export([ctx/1, ctx/2]).

-include("common.hrl").
-include("protocol.hrl").
-include("schema.hrl").

-record(pdata, {
    pid,
    self,
    agent,
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

init([R = #tab_player_info{agent = Agent, pid = PId, identity = Identity, nick = Nick, photo = Photo}]) ->
  process_flag(trap_exit, true),
  ok = create_runtime(PId, self()),
  {ok, #pdata{agent = Agent, pid = PId, self = self(), nick = list_to_binary(Nick), photo = list_to_binary(Photo), identity = Identity, record = R}}.

%% player watch game
handle_cast(#cmd_watch{game = G}, Data = #pdata{identity = Identity}) when is_pid(G) ->
  game:watch(G, Identity),
  {noreply, Data#pdata{ watching = G}};

handle_cast(R = #cmd_watch{}, Data = #pdata{}) ->
  ?LOG([{player, {error, watch}}, {join, R}, {pdata, Data}]),
  {noreply, Data};

%% player unwatch game
handle_cast(#cmd_unwatch{game = G}, Data = #pdata{watching = W, playing = P}) when P /= ?UNDEF; W /= G ->
  {noreply, Data};

handle_cast(#cmd_unwatch{game = G}, Data = #pdata{identity = Identity}) ->
  game:unwatch(G, Identity),
  {noreply, Data#pdata{ watching = ?UNDEF}};

%% player join game
handle_cast(#cmd_join{game = G, buyin = B}, Data = #pdata{watching = W, playing = P, record = R}) when is_pid(G), W =:= G, P =:= ?UNDEF, (R#tab_player_info.cash + R#tab_player_info.credit) < B  ->
  notify(#notify_error{error = ?ERR_JOIN_LESS_BALANCE}),
  {noreply, Data};

handle_cast(R = #cmd_join{game = G}, Data = #pdata{watching = W, playing = P}) when is_pid(G), W =:= G, P =:= ?UNDEF ->
  game:join(G, R#cmd_join{pid = Data#pdata.pid, agent = Data#pdata.agent, identity = Data#pdata.identity, nick = Data#pdata.nick, photo = Data#pdata.photo}),
  {noreply, Data};

handle_cast(R = #cmd_join{game = G}, Data = #pdata{identity = Identity, watching = W, playing = P}) when is_pid(G), W =:= ?UNDEF, P =:= ?UNDEF ->
  game:watch(G, Identity),
  handle_cast(R, Data#pdata{watching = G});

handle_cast(R = #cmd_join{}, Data = #pdata{}) ->
  ?LOG([{player, {error, join}}, {join, R}, {pdata, Data}]),
  {noreply, Data};

%% player leave game
handle_cast(R = #cmd_leave{game = G}, Data = #pdata{playing = P, playing_sn = SN}) when G =:= P, SN /= 0 ->
  game:leave(G, R#cmd_leave{agent = Data#pdata.agent, pid = Data#pdata.pid, sn = SN}),
  {noreply, Data};

handle_cast(#cmd_leave{}, Data) ->
  {noreply, Data};

%% player info query
handle_cast(#cmd_query_player{}, Data = #pdata{}) ->
  R = #notify_player{
    player = Data#pdata.pid,
    nick = Data#pdata.nick,
    photo = Data#pdata.photo
  },
  handle_cast({notify, R}, Data);

handle_cast(#cmd_query_balance{}, Data) ->
  R = #notify_acount{ balance = 0, inplay = 0 },
  handle_cast({notify, R}, Data);

handle_cast(#cmd_query_seats{game = Game}, Data) ->
  game:query_seats(Game),
  {noreply, Data};

handle_cast({notify, R = #notify_join{proc = G, player = P}}, Data = #pdata{pid = PId}) when P =:= PId ->
  Info = reload_player_info(PId),
  forward_to_client(R, Data),
  {noreply, Data#pdata{ playing = G, playing_sn = R#notify_join.sn, record = Info }};

handle_cast({notify, R = #notify_leave{player = P}}, Data = #pdata{pid = PId}) when P =:= PId ->
  Info = reload_player_info(PId),
  forward_to_client(R, Data),
  {noreply, Data#pdata{ playing = ?UNDEF, playing_sn = 0, record = Info }};

handle_cast({notify, R}, Data) ->
  forward_to_client(R, Data),
  {noreply, Data};

handle_cast(stop, Data) ->
  {stop, normal, Data};

handle_cast({stop, Reason}, Data) ->
  {stop, Reason, Data};

handle_cast(R = #cmd_raise{game = G}, Data = #pdata{playing = P}) when G =:= P ->
  game:raise(G, R#cmd_raise{sn = Data#pdata.playing_sn, pid = Data#pdata.pid}),
  {noreply, Data};

handle_cast(R = #cmd_fold{game = G}, Data = #pdata{playing = P}) when G =:= P ->
  game:fold(G, R#cmd_fold{pid = Data#pdata.pid}),
  {noreply, Data};

handle_cast(R, Data) ->
  ?LOG([{unknown_cast, R}]),
  {noreply, Data}.

handle_call(ctx, _From, Data) ->
  {reply, Data, Data};

handle_call(info, _From, Data = #pdata{record = R}) ->
  {reply, R, Data};

handle_call({client, Client}, _From, Data) when is_pid(Client) ->
  {reply, Client, Data#pdata{ client = Client}};

handle_call(kill, _From, Data) ->
  {stop, normal, ok, Data};

handle_call(pdata, _From, Data) ->
  {reply, Data, Data};

handle_call(R, From, Data) ->
  error_logger:info_report([{module, ?MODULE}, {process, self()}, {unknown_call, R}, {from, From}]),
  {noreply, Data}.

terminate(_Reason, Data) ->
  ok = mnesia:dirty_delete(tab_player, Data#pdata.pid).

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

start(Identity) when is_list(Identity) ->
  [R] = mnesia:dirty_index_read(tab_player_info, Identity, identity),
  start(R);
  
start(R = #tab_player_info{pid = PId}) ->
  gen_server:start(?PLAYER(PId), player, [R], []).

stop(Identity) when is_list(Identity) ->
  gen_server:cast(?PLAYER(Identity), stop).

stop(Identity, Reason) when is_list(Identity) ->
  gen_server:cast(?PLAYER(Identity), {stop, Reason}).

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
  gen_server:call(Player, logout).

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

forward_to_client(_, #pdata{client = Client}) when Client =:= ?UNDEF -> exit(undefined_client);
forward_to_client(R, #pdata{client = Client}) -> client:send(Client, R).

%%%
%%% unit test
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%start_all_test() ->
  %setup(),
  %start("player_1"),
  %?assertEqual(true, erlang:is_process_alive(?LOOKUP_PLAYER(1))),
  %Pdata = pdata(1),
  %?assertEqual(<<"player1">>, Pdata#pdata.nick),
  %?assertEqual(<<"default1">>, Pdata#pdata.photo),
  %[Xref] = mnesia:dirty_read(tab_player, Pdata#pdata.pid),
  %?assertEqual(Pdata#pdata.self, Xref#tab_player.process),
  %?assertEqual(undefined, Xref#tab_player.socket).

start_test() ->
  setup(),
  {ok, _Pid} = start("player_1"),
  ?assertEqual(true, erlang:is_process_alive(?LOOKUP_PLAYER(1))),
  Pdata = pdata(1),
  ?assertEqual(<<"player1">>, Pdata#pdata.nick),
  ?assertEqual(<<"default1">>, Pdata#pdata.photo),
  [Xref] = mnesia:dirty_read(tab_player, Pdata#pdata.pid),
  ?assertEqual(Pdata#pdata.self, Xref#tab_player.process),
  ?assertEqual(undefined, Xref#tab_player.socket).

%auth_test() ->
  %setup(),
  %[R] = mnesia:dirty_index_read(tab_player_info, "player_1", identity),
  %?assertEqual({ok, pass, R}, player:auth("player_1", ?DEF_PWD)),
  %?assertEqual({ok, unauth}, player:auth("player_nil", ?DEF_PWD)),
  %?assertEqual({ok, unauth}, player:auth("player_1", "bad_pwd")),

  %Info = #tab_player_info{identity = "player_1", password = ?DEF_HASH_PWD, disabled = false},
  %?assertEqual({ok, pass, Info}, player:auth(Info, ?DEF_PWD)),
  %?assertEqual({ok, unauth}, player:auth(Info, "bad_pwd")),
  %?assertEqual({ok, player_disable}, player:auth(Info#tab_player_info{disabled = true}, ?DEF_PWD)).

setup() ->
  schema:init(),

  Players = [
    #tab_player_info {
      pid = 1,
      identity = "player_1",
      password = ?DEF_HASH_PWD,
      nick = "player1",
      photo = "default1",
      agent = "root"
    }, #tab_player_info {
      pid = 2,
      identity = "player_2",
      nick = "player2",
      agent = "root"
    }, #tab_player_info {
      pid = 3,
      identity = "player_3",
      nick = "player3",
      agent = "agent_1"
    }, #tab_player_info {
      pid = 4,
      identity = "player_4",
      nick = "player4",
      agent = "agent_1_1"
    }
  ],

  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Players),
  
  Fun = fun(#tab_player_info{pid = PId}, _Acc) ->
      case ?LOOKUP_PLAYER(PId) of
        Pid when is_pid(Pid) -> 
          ok = gen_server:call(Pid, kill);
        _ -> ok
      end
  end,
  lists:foldl(Fun, nil, Players).

pdata(Id) ->
  gen_server:call(?PLAYER(Id), pdata).
-endif.
