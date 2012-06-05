-record(tab_player_info, {
    pid,
    identity,
    password,
    nick,
    photo,
    login_errors = 0,
    disabled = false,
    cash = 0,
    credit = 0
  }).

-record(tab_inplay, {
    pid,
    inplay
  }).

-record(tab_turnover_log, {
    id = now(),
    pid,      %% pid
    game,     %% {gid, sn}
    amt,      %% amt
    cost,     %% winner cost amt
    inplay,   %% in out result inplay
    date = date(),     %% {year, month, day}
    time = time()      %% {hour, min, sec}
  }).

-record(tab_buyin_log, {
    id = now(), 
    pid,      %% pid
    gid,      %% gid
    amt,      %% amt
    cash,     %% cash result by change amt
    credit,   %% credit
    date = date(),     %% {year, month, day}
    time = time()      %% {hour, min, sec}
  }).

-record(tab_counter, {
    type,
    value
  }).

-record(tab_player, {
    pid                 ::integer(),
    process = undefined ::pid() | undefined,
    socket = undefined  ::pid() | undefined 
  }).


-record(tab_game_config, {
    id,
    module,
    mods,
    limit,
    seat_count,
    start_delay,
    required,
    timeout,
    max
  }).

-record(tab_game_xref, {
    gid,
    process,
    module,
    limit,
    seat_count,
    timeout,
    required % min player count 
  }).
