-module(agent).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-export([start/0, auth/2, create/2, turnover/1, turnover/2, betting/3, subordinate/1, players/1, sum_info/1]).

-include("common.hrl").
-include("schema.hrl").

-include_lib("stdlib/include/qlc.hrl").

-record(ply, {
    pid = ?UNDEF,
    cash = 0,
    credit = 0
  }).

-record(sub, {
    identity = ?UNDEF,
    cash = 0,
    credit = 0,
    balance = 0,
    sub_count = 0,
    ply_count = 0,
    turnover = 0,
    turnover_today = 0
  }).

-record(pdata, {
    aid = ?UNDEF,
    level = 1,
    identity,
    cash = 0,
    credit = 0,
    turnover_daily = [],              %% proplist [{date, turnover},...]
    exp_collect_subordinate = [],     %% identity
    last_collect_time = now(),
    subordinate = [],                 %% #sub
    players = [],                     %% #ply
    disable = false,
    password = ?UNDEF,
    parent = ?UNDEF,
    sum = ?UNDEF                      %% myself #sub
  }).

%% Server Function

init([R = #tab_agent{}]) ->
  process_flag(trap_exit, true),

  Agents = mnesia:dirty_index_read(tab_agent, R#tab_agent.identity, parent),
  Subordinate = lists:map(fun(#tab_agent{identity = Identity}) -> #sub{identity = Identity} end, Agents),
  ExpSubordinate = lists:map(fun(#sub{identity = Identity}) -> Identity end, Subordinate),

  {ok, #pdata { 
    aid = R#tab_agent.aid,
    identity = R#tab_agent.identity,
    cash = R#tab_agent.cash,
    credit = R#tab_agent.credit,
    disable = R#tab_agent.disable,
    players = reload_players(R#tab_agent.identity),
    password = R#tab_agent.password,
    subordinate = Subordinate,
    exp_collect_subordinate = ExpSubordinate,
    turnover_daily = reload_turnover(R#tab_agent.identity),
    parent = R#tab_agent.parent
  }}.

handle_cast(collect, Data = #pdata{subordinate = Sub, exp_collect_subordinate = ExpSub})
when Sub =:= []; ExpSub =:= [] ->
  {noreply, report(Data)};

handle_cast(collect, Data = #pdata{}) -> 
  ExpSub = lists:map(
    fun(#sub{identity = Identity}) -> 
        collect(?LOOKUP_AGENT(Identity)),
        Identity
    end, Data#pdata.subordinate),
  {noreply, Data#pdata{exp_collect_subordinate = ExpSub}};

handle_cast({report, R = #sub{identity = Identity}}, Data = #pdata{}) ->
  NewData = case lists:member(Identity, Data#pdata.exp_collect_subordinate) of
    true ->
      Sub = lists:keyreplace(Identity, 1, Data#pdata.subordinate, R),
      ExpSub = lists:delete(Identity, Data#pdata.exp_collect_subordinate),
      Data#pdata{subordinate = Sub, exp_collect_subordinate = ExpSub};
    _ ->
      Data
  end,

  case NewData#pdata.exp_collect_subordinate of
    [] ->
      {noreply, report(NewData)};
    _ ->
      {noreply, NewData}
  end;

handle_cast({betting, Date, Amt}, Data = #pdata{turnover_daily = Daily}) ->
  case proplists:lookup(Date, Daily) of
    none ->
      {noreply, Data#pdata{turnover_daily = Daily ++ [{Date, Amt}]}};
    {Date, Sum} ->
      {noreply, Data#pdata{turnover_daily = lists:keyreplace(Date, 1, Daily, {Date, Sum + Amt})}}
  end;
  
handle_cast(_Msg, Agent) ->
  {noreply, Agent}.

handle_call({auth, ReqPwd}, _From, Data = #pdata{password = Pwd}) ->
  case ReqPwd of
    Pwd ->
      {reply, true, Data};
    _ ->
      {reply, false, Data}
  end;

handle_call(turnover, _From, Data) ->
  {reply, Data#pdata.turnover_daily, Data};

handle_call({turnover, today}, _From, Data) ->
  case proplists:lookup(date(), Data#pdata.turnover_daily) of
    none ->
      {reply, {date(), 0}, Data};
    Today ->
      {reply, Today, Data}
  end;

handle_call(subordinate, _From, Data) ->
  {reply, Data#pdata.subordinate, Data};

handle_call(players, _From, Data) ->
  {reply, Data#pdata.players, Data};

handle_call(sum_info, _From, Data) ->
  {reply, Data#pdata.sum, Data};

handle_call({create, R = #tab_player_info{}}, _Form, Data = #pdata{}) ->
  Fun = fun() ->
      Amt = R#tab_player_info.cash + R#tab_player_info.credit,
      ok = mnesia:write_lock_table(tab_player_info),

      [Agent] = mnesia:wread(tab_agent, Data#pdata.aid),
      ok = check_identity(R),
      ok = check_amt(Agent, Amt),

      PId = counter:bump(player),

      mnesia:write(Agent#tab_agent{cash = Agent#tab_agent.cash - Amt}),
      mnesia:write(R#tab_player_info{
          pid = counter:bump(player),
          password = erlang:md5(R#tab_player_info.password)}),

      {ok, #ply{
          pid = PId,
          cash = R#tab_player_info.cash,
          credit = R#tab_player_info.credit
        }}
  end, 

  case mnesia:transaction(Fun) of
    {atomic, {ok, Ply}} ->
      {reply, ok, Data#pdata{players = Data#pdata.players ++ [Ply]}};
    {abort, Reason} ->
      {reply, Reason, Data}
  end;

handle_call({create, R = #tab_agent{identity = Identity, parent = Parent}}, _Form, Data = #pdata{identity = Parent}) ->
  Fun = fun() ->
      ok = mnesia:write_lock_table(tab_agent),

      [Agent] = mnesia:index_read(tab_agent, Parent, identity),
      ok = check_identity(R),
      ok = check_amt(Agent, R#tab_agent.cash + R#tab_agent.credit),

      DesignateR = R#tab_agent{aid = counter:bump(agent), password = erlang:md5(R#tab_agent.password)},

      mnesia:write(Agent#tab_agent{cash = Agent#tab_agent.cash - (R#tab_agent.cash + R#tab_agent.credit)}),
      mnesia:write(DesignateR),

      gen_server:start_link(?AGENT(Identity), ?MODULE, [DesignateR], []),

      {ok, #sub{
          identity = Identity, 
          cash = R#tab_agent.cash, credit = R#tab_agent.credit,
          balance = R#tab_agent.cash + R#tab_agent.credit }}
  end,

  case mnesia:transaction(Fun) of
    {atomic, {ok, Sub}} ->
      {reply, ok, Data#pdata{subordinate = Data#pdata.subordinate ++ [Sub]}};
    {abort, Reason} ->
      {reply, Reason, Data}
  end;

handle_call(_Msg, _From, Agent) ->
  {noreply, Agent}.

handle_info(_Msg, Server) ->
  {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

terminate(normal, _Server) ->
  ok.

%% Private Function

report(Data = #pdata{}) ->
  [Agent] = mnesia:dirty_index_read(tab_agent, Data#pdata.identity, identity),
  ReloadData = Data#pdata{
    cash = Agent#tab_agent.cash,
    credit = Agent#tab_agent.credit
  },

  Sum = sum(ReloadData),
  ExpSub = lists:map(fun(#sub{identity = Identity}) -> Identity end, ReloadData#pdata.subordinate),

  case ReloadData#pdata.identity of
    "root" -> ok;
    _ -> gen_server:cast(?LOOKUP_AGENT(ReloadData#pdata.parent), {report, Sum})
  end,

  ReloadData#pdata{sum = Sum, exp_collect_subordinate = ExpSub}.

sum(Data = #pdata{subordinate = Sub, cash = Cash, credit = Credit, players = Ply, turnover_daily = Daily}) ->
  {SubTurnoverToday, SubTurnover} = sum_turnover(Sub),

  #sub{
    identity = Data#pdata.identity,
    cash = Data#pdata.cash,
    credit = Data#pdata.credit,
    balance = sum_balance(Sub) + sum_balance(Ply) + Cash + Credit,
    ply_count = length(Ply),
    sub_count = length(Sub),
    turnover = SubTurnover + get_turnover(Daily, 7),
    turnover_today = SubTurnoverToday + get_turnover(Daily)
  }.

sum_balance([]) -> 0;
sum_balance(L = [#sub{}|_]) ->
  lists:sum(lists:map(fun(#sub{balance = B}) -> B end, L));
sum_balance(L = [#ply{}|_]) ->
  lists:sum(lists:map(fun(R) -> R#ply.cash + R#ply.credit end, L)).

sum_turnover([])  -> {0, 0};
sum_turnover(L = [#sub{}|_]) ->
  {lists:sum(lists:map(fun(#sub{turnover_today = T}) -> T end, L)), lists:sum(lists:map(fun(#sub{turnover = T}) -> T end, L))}.

get_turnover(Daily) ->
  get_turnover(Daily, 0, 0).

get_turnover(Daily, N) ->
  get_turnover(Daily, N, 0).

get_turnover(_Daily, N, Sum) when N < 0 -> Sum;
get_turnover(Daily, N, Sum) ->
  case proplists:lookup(shift_date(N), Daily) of
    {_Date, Turnover} ->
      get_turnover(Daily, N - 1, Sum + Turnover);
    none ->
      get_turnover(Daily, N - 1, Sum)
  end.

reload_players(Agent) ->
  Players = mnesia:dirty_index_read(tab_player_info, Agent, agent),
  lists:map(fun(#tab_player_info{pid = PId, cash = Cash, credit = Credit}) -> #ply{pid = PId, cash = Cash, credit = Credit} end, Players).

reload_turnover(Agent) ->
  F = fun() ->
      Query = qlc:q(
        [{T#tab_turnover_log.date, T#tab_turnover_log.amt} || 
          T <- mnesia:table(tab_turnover_log),
          T#tab_turnover_log.aid =:= Agent, T#tab_turnover_log.date >= shift_date(-30)
        ]
      ),

      qlc:e(Query) 
  end,

  {atomic, Logs} = mnesia:transaction(F),
  group_turnover_log(Logs).

group_turnover_log(Logs) -> 
  Result = group_turnover_log(Logs, ?UNDEF, ?UNDEF, []),
  case proplists:lookup(date(), Result) of
    none ->
      Result ++ [{date(), 0}];
    _ ->
      Result
  end.

group_turnover_log([], ?UNDEF, ?UNDEF, Daily) -> Daily;
group_turnover_log([], Date, Sum, Daily) -> Daily ++ [{Date, Sum}];
group_turnover_log([{Date, Amt}|T], ?UNDEF, ?UNDEF, Daily) ->
  group_turnover_log(T, Date, Amt, Daily);
group_turnover_log([{Date, Amt}|T], Date, Sum, Daily) ->
  group_turnover_log(T, Date, Amt + Sum, Daily);
group_turnover_log(L = [{_Date, _Amt}|_T], OldDate, Sum, Daily) ->
  group_turnover_log(L, ?UNDEF, ?UNDEF,  Daily ++ [{OldDate, Sum}]).
  
shift_date(N) ->
  Days = calendar:date_to_gregorian_days(date()),
  calendar:gregorian_days_to_date(Days + N).

check_amt(#tab_agent{cash = Cash, credit = Credit}, Amt) when Amt > (Cash + Credit) ->
  exit(less_balance);
check_amt(#tab_agent{}, _) -> 
  ok.

check_identity(#tab_player_info{identity = Identity}) ->
  case mnesia:read_index(tab_player_info, Identity, identity) of
    [] -> ok;
    _ -> exit(repeat_identity)
  end;

check_identity(#tab_agent{identity = Identity}) ->
  case mnesia:read_index(tab_agent, Identity, identity) of
    [] -> ok;
    _ -> exit(repeat_identity)
  end.

%% Client Function

start() ->
  ok = mnesia:wait_for_tables([tab_agent], 1000),
  {atomic, Agents} = mnesia:transaction(fun() -> qlc:e(mnesia:table(tab_agent)) end),

  lists:map(
    fun(R = #tab_agent{identity = Identity}) -> 
        case ?LOOKUP_AGENT(Identity) of
          PID when is_pid(PID) ->
            exit(PID, normal),
            timer:sleep(500);
          _ -> ok
        end,
        gen_server:start_link(?AGENT(Identity), ?MODULE, [R], [])
    end, Agents),

  collect(?LOOKUP_AGENT("root")).

auth(Identity, Password) when is_list(Identity), is_list(Password) ->
  gen_server:call(?LOOKUP_AGENT(Identity), {auth, erlang:md5(Password)}).

betting(Identity, Date, Amt) when is_list(Identity) ->
  betting(?LOOKUP_AGENT(Identity), Date, Amt);
betting(Proc, Date, Amt) when is_pid(Proc) ->
  gen_server:cast(Proc, {betting, Date, Amt}).

turnover(Identity) when is_list(Identity) ->
  gen_server:call(?LOOKUP_AGENT(Identity), turnover).

turnover(Identity, today) when is_list(Identity) ->
  gen_server:call(?LOOKUP_AGENT(Identity), {turnover, today}).

subordinate(Identity) when is_list(Identity) ->
  gen_server:call(?LOOKUP_AGENT(Identity), subordinate).

players(Identity) when is_list(Identity) ->
  gen_server:call(?LOOKUP_AGENT(Identity), players).

sum_info(Identity) when is_list(Identity) ->
  gen_server:call(?LOOKUP_AGENT(Identity), sum_info).

create(Identity, R) ->
  gen_server:call(?AGENT(Identity), {create, R}).

collect(Proc) ->
  gen_server:cast(Proc, collect).

%% Eunit Test Case

%subordinate_test() ->
  %setup(),
  %?assertEqual(["agent_1_1"], subordinate("agent_1")),
  %?assertEqual(["agent_1", "disable_agent"], subordinate("root")).

%create_player_test() ->
  %setup(),
  %?assertEqual(repeat_identity, create("agent_1", #tab_player_info{identity = "player_1", agent = "agent_1"}, {0, 1000})),
  %?assertEqual(less_balance, create("agent_1", #tab_player_info{identity = "player_new", agent = "agent_1"}, {1, 10000})),
  %?assertEqual(10000, balance("agent_1")),
  %?assertEqual(ok, create("agent_1", #tab_player_info{identity = "player_new", agent = "agent_1", password = "def_pwd"}, {1000, 1000})),
  %?assertEqual(8000, balance("agent_1")),
  %?assertEqual(["player_3", "player_new"], players("agent_1")),
  %[NewPlayer] = mnesia:dirty_index_read(tab_player_info, "player_new", identity),
  %?assertEqual(0, NewPlayer#tab_player_info.cash).

%create_test() ->
  %setup(),
  %?assertEqual(repeat_identity, create("agent_1", #tab_agent{identity = "agent_1_1", password = ?DEF_PWD, parent = "agent_1"})),
  %?assertEqual(less_balance, create("agent_1", #tab_agent{identity = "agent_1_new", password = ?DEF_PWD, parent = "agent_1", cash = 1000 * 10000, credit = 0})),

  %?assertEqual(10000, balance("agent_1")),
  %?assertEqual(ok, create("agent_1", #tab_agent{identity = "agent_1_new", password = ?DEF_PWD, parent = "agent_1", cash = 1000, credit = 1000})),
  %?assertEqual(8000, balance("agent_1")),
  %?assert(is_pid(?LOOKUP_AGENT("agent_1_new"))),
  %?assertEqual(["agent_1_1", "agent_1_new"], subordinate("agent_1")).

%players_test() ->
  %setup(),
  %?assertEqual(["player_1", "player_2"], players("root")),
  %?assertEqual(["player_3"], players("agent_1")),
  %?assertEqual(["player_4"], players("agent_1_1")).

%betting_test() ->
  %setup(),
  %?assertEqual(not_own_player, betting("root", "player_3", 10)),
  %?assertEqual(ok, betting("root", "player_1", 10)),
  %?assertEqual(ok, betting("root", "player_2", 10)),
  %?assertEqual(ok, betting("root", "player_2", 30)),
  %?assertEqual(50, turnover("root", today)).

%auth_test() ->
  %setup(),
  %?assert(true =:= auth("root", ?DEF_PWD)),
  %?assert(false =:= auth("root", "")).

%balance_test() ->
  %setup(),
  %?assertEqual(10000, balance("agent_1")).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
  setup(),
  agent:start(),
  ?assert(is_pid(?LOOKUP_AGENT("root"))),
  ?assert(is_pid(?LOOKUP_AGENT("agent1"))),
  ?assert(is_pid(?LOOKUP_AGENT("agent2"))),
  ?assert(is_pid(?LOOKUP_AGENT("agent3"))),

  timer:sleep(500),

  ?assertMatch(#sub{sub_count = 3, ply_count = 0}, agent:sum_info("root")),
  ?assertMatch(#sub{sub_count = 0, ply_count = 1}, agent:sum_info("agent1")),
  ?assertMatch(#sub{sub_count = 0, ply_count = 1}, agent:sum_info("agent2")),
  ?assertMatch(#sub{sub_count = 0, ply_count = 1}, agent:sum_info("agent3")).

generate_test() ->
  Agents = [#tab_agent{
      aid = Id, 
      identity = "agent" ++ integer_to_list(Id), 
      password = ?DEF_HASH_PWD,
      parent = Parent,
      disable = Disable} 
    || Id <- lists:seq(1,3), Parent <- ["root"], Disable <- [false]],

  [H|_] = Agents,
  ?assertEqual(3, length(Agents)),
  ?assertMatch(#tab_agent{aid = 1, parent = "root", disable = false, identity = "agent1"}, H),
  ?assertMatch(#tab_agent{aid = 3, parent = "root", disable = false, identity = "agent3"}, lists:last(Agents)).

group_turnover_log_test() ->
  NowDate = date(),
  Data = [{Date, Amt} || Date <- [{2012, 1, 1}, {2012, 1, 2}], Amt <- [5, 10]],
  ?assertMatch([{{2012, 1, 1}, 15}, {{2012, 1, 2}, 15}, {NowDate, 0}], group_turnover_log(Data)),
  ?assertMatch([{{2012, 1, 1}, 15}, {{2012, 1, 2}, 15}, {{2012, 1, 3}, 5}, {NowDate, 0}], group_turnover_log(Data ++ [{{2012, 1, 3}, 5}])).

reload_turnover_log_empty_test() ->
  schema:init(),
  DateNow = date(),
  ?assertMatch([{DateNow, 0}], reload_turnover("root")).

reload_turnover_log_test() ->
  schema:init(),
  Logs = [#tab_turnover_log{aid = "root", date = Date, amt = Amt} || Date <- [shift_date(-40), shift_date(-10)], Amt <- [10, 5]],
  Logs1 = Logs ++ [#tab_turnover_log{aid = "test", date = shift_date(-10), amt = 10}], %% add other user check aid condition
  Logs2 = Logs1 ++ [#tab_turnover_log{aid = "root", date = shift_date(-1), amt = 10}],
  lists:map(fun(R) -> mnesia:dirty_write(R) end, Logs2),
  
  DateNow = date(),
  DateBeforTen = shift_date(-10),
  DateBeforOne = shift_date(-1),

  ?assertMatch([{DateBeforOne, 10}, {DateBeforTen, 15}, {DateNow, 0}], reload_turnover("root")).

betting_turnover_today_test() ->
  setup(),
  agent:start(),

  Date = date(),
  BeforDate = shift_date(-1),

  ?assertMatch({Date, 0}, turnover("root", today)),
  betting("root", Date, 10),
  ?assertMatch({Date, 10}, turnover("root", today)),
  ?assertMatch([{Date, 10}], turnover("root")),
  betting("root", BeforDate, 10),
  ?assertMatch({Date, 10}, turnover("root", today)),
  ?assertMatch([{Date, 10}, {BeforDate, 10}], turnover("root")).

collect_test() ->
  setup(),

  %% generate many players for root agent.
  Players = [#tab_player_info{
      pid = Id,
      identity = "player" ++ integer_to_list(Id),
      password = ?DEF_HASH_PWD,
      agent = "root"}
    || Id <- lists:seq(11,13)],

  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Players),

  agent:start(),
  timer:sleep(500),
  ?assertMatch(#sub{sub_count = 3, ply_count = 3}, agent:sum_info("root")),
  ?assertMatch(#sub{sub_count = 0, ply_count = 1}, agent:sum_info("agent1")),
  ?assertMatch(#sub{sub_count = 0, ply_count = 1}, agent:sum_info("agent2")),
  ?assertMatch(#sub{sub_count = 0, ply_count = 1}, agent:sum_info("agent3")).

setup() ->
  schema:init(),

  Agents = [#tab_agent{
      aid = Id, 
      identity = "agent" ++ integer_to_list(Id rem 10), 
      password = ?DEF_HASH_PWD,
      parent = Parent,
      disable = Disable} 
    || Id <- lists:seq(11, 13), Parent <- ["root"], Disable <- [false]],

  Players = [#tab_player_info{
      pid = Id,
      identity = "player" ++ integer_to_list(Id),
      password = ?DEF_HASH_PWD,
      agent = "agent" ++ integer_to_list(Id)} 
    || Id <- lists:seq(1,3)],

  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Agents),
  lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Players).

-endif.
