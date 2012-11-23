-module(op_phantom).
-behavior(gen_server).

-export([start_link/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).
-export([send/2]).
-include("genesis.hrl").

%%
%% Client API
%%

-record(pd, { pid, gid, sn }).

start_link(PID, GID, SN) when is_integer(PID), is_pid(GID), is_integer(SN) ->
  gen_server:start_link(?MODULE, [PID, GID, SN], []).

send(P, R) ->
  gen_server:cast(P, R).

%%
%% Callback Functions
%%

init([PID, GID, SN]) ->
  error_logger:info_report([{phantom, create}]),
  {ok, #pd{pid = PID, gid = GID, sn = SN}}.

terminate(_Reason, _LoopData) ->
  ok.

handle_info(_Info, LoopData) ->
  {noreply, LoopData}.

code_change(_OldVsn, LoopData, _Extra) ->
  {ok, LoopData}.

handle_call(_Msg, _From, LoopData) ->
  {reply, ok, LoopData}.

handle_cast(#notify_game_cancel{}, LoopData) ->
  handle_cast(#notify_game_end{}, LoopData);

handle_cast(#notify_game_end{}, LoopData = #pd {pid = PID, gid = GID}) ->
  error_logger:info_report([{phantom, notify_game_over}]),
  player:leave(PID, GID),
  {noreply, LoopData};

handle_cast(#notify_leave{sn = SN}, LoopData = #pd{pid = PID, sn = SN}) ->
  %% TODO 这里可以处理的更好一些，而不是简单粗暴的将进程杀掉。
  genesis_players_sup:terminate_child_ex(PID),
  error_logger:info_report([{phantom, terminate_child}]),
  {noreply, LoopData};
  
handle_cast(#notify_betting{}, LoopData = #pd{pid = PID, gid = GID}) ->
  error_logger:info_report([{phantom, notify_betting}]),
  player:fold(PID, GID),
  {noreply, LoopData};

handle_cast(_R, LoopData) ->
  {noreply, LoopData}.
