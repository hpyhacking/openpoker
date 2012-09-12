%% Game Notify Protocol
-define(NOTIFY_ACOUNT,        100).
-record(notify_acount,        { balance, inplay }).

-define(NOTIFY_GAME,          101).
-record(notify_game,          { game, name, limit, seats, require, joined }).

-define(NOTIFY_GAME_DETAIL,   102).
-record(notify_game_detail,   { game, pot, stage, limit, seats, require, joined }).

-define(NOTIFY_GAME_START,    103).
-record(notify_game_start,    { game }).

-define(NOTIFY_GAME_END,      104).
-record(notify_game_end,      { game }).  

-define(NOTIFY_GAME_CANCEL,   105).
-record(notify_game_cancel,   { game }).  

%% Texas Notify Protocol
-define(NOTIFY_STAGE,         106).
-record(notify_stage,         { game, stage }).

-define(NOTIFY_STAGE_END,     107).
-record(notify_stage_end,     { game, stage }).

-define(NOTIFY_JOIN,          108).
-record(notify_join,          { game, player, sn, buyin, nick, photo, '|', proc }).

-define(NOTIFY_LEAVE,         109).
-record(notify_leave,         { game, sn, player, '|', proc }).

-define(NOTIFY_BUTTON,        110).
-record(notify_button,        { game, b}).

-define(NOTIFY_SB,            111).
-record(notify_sb,            { game, sb }).

-define(NOTIFY_BB,            112).
-record(notify_bb,            { game, bb }).

-define(NOTIFY_RAISE,         114).
-record(notify_raise,         { game, player, raise, call }).

-define(NOTIFY_SEAT,          115).
-record(notify_seat,          { game, sn, state, player, inplay, bet, nick, photo }).

-define(NOTIFY_ACTOR,         116).
-record(notify_actor,         { game, sn }).  

-define(NOTIFY_BETTING,       117).
-record(notify_betting,       { game, call, min, max }).

-define(NOTIFY_DRAW,          118).
-record(notify_draw,          { game, player, card }).

-define(NOTIFY_PRIVATE,       119).
-record(notify_private,       { game, player, card }).

-define(NOTIFY_SHARED,        120).
-record(notify_shared,        { game, card }).  

-define(NOTIFY_HAND,          121).
-record(notify_hand,          { game, player, rank, high1, high2, suit}).

-define(NOTIFY_CARDS,         122).
-record(notify_cards,         { game, player, cards }).

-define(NOTIFY_WIN,           123).
-record(notify_win,           { game, player, amount }).

-define(NOTIFY_PLAYER,        124).
-record(notify_player,        { player, nick, photo }).

-define(NOTIFY_FOLD,          125).
-record(notify_fold,          { game, sn }).

-define(NOTIFY_OUT,           126).
-record(notify_out,           { game, player }).

-define(NOTIFY_GAMES_LIST_END,127).
-record(notify_games_list_end,{ size }).

-define(NOTIFY_SEATS_LIST_END,128).
-record(notify_seats_list_end,{ size }).

-define(NOTIFY_ERROR,         255).
-record(notify_error,         { error }).

