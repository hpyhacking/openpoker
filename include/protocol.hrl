-define(ERR_UNAUTH,                 1).
-define(ERR_DATA,                   2).
-define(ERR_PROTOCOL,               3).
-define(ERR_START_DISABLED,         4).
-define(ERR_CONNECTION_TIMEOUT,     5).
-define(ERR_AGENT_DISABLE,          6).
-define(ERR_PLAYER_DISABLE,         7).
-define(ERR_JOIN_LESS_BALANCE,      8).

%% Game Commands
-define(CMD_LOGIN,            10).
-record(cmd_login,            { identity, password }).

-define(CMD_LOGOUT,           11).
-record(cmd_logout,           {}).

-define(CMD_QUERY_PLAYER,     12).
-record(cmd_query_player,     { player }).

-define(CMD_QUERY_BALANCE,    13).
-record(cmd_query_balance,    {}).  

-define(CMD_QUERY_GAME,       14).
-record(cmd_query_game,       {}).

%% Texas Commands
-define(CMD_WATCH,            31).
-record(cmd_watch,            { game, '|', player }).

-define(CMD_UNWATCH,          32).
-record(cmd_unwatch,          { game, '|', player }).

-define(CMD_JOIN,             33).
-record(cmd_join,             { game, sn, buyin, '|', pid, identity, nick, photo, agent, proc }).

-define(CMD_LEAVE,            34).
-record(cmd_leave,            { game, '|', pid, sn, agent }).

-define(CMD_RAISE,            35).
-record(cmd_raise,            { game, amount, '|', pid, sn }).

-define(CMD_FOLD,             36).
-record(cmd_fold,             { game, '|', pid }).

-define(CMD_QUERY_SEATS,      37).
-record(cmd_query_seats,      { game }).

-define(CMD_OUT,              38).
-record(cmd_out,              { game, sn, buyin, '|', pid, proc }).

-include("notify.hrl").
