-module(op_games_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-include("openpoker.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Fun = fun(R, Acc) -> gen(R, []) ++ Acc end, 
  ok = mnesia:wait_for_tables([tab_game_config], ?WAIT_TABLE),
  {atomic, NewAcc} = mnesia:transaction(fun() ->
    mnesia:foldl(Fun, [], tab_game_config)
  end),
  {ok, {{one_for_one, 5, 10}, lists:reverse(NewAcc)}}.

start_child(Conf = #tab_game_config{}) ->
  Fun = fun(Game) -> supervisor:start_child(?MODULE, Game) end,
  lists:map(Fun, gen(Conf, [])).

gen(#tab_game_config{max = 0}, Acc) -> Acc;
gen(Conf = #tab_game_config{module = Module, mods = Mods, max = N}, Acc) ->
  StartMod = {op_exch, start_link, [Module, Conf, Mods]},
  NewAcc = [{make_ref(), StartMod, permanent, 2000, worker, []}] ++ Acc,
  gen(Conf#tab_game_config{max = N - 1}, NewAcc).
