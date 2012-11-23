-module(betting).
-export([start/2, betting/2]).

-include("genesis.hrl").

-define(ZERO, 0).

%%%
%%% callback
%%%

start([?GS_PREFLOP], Ctx = #texas{bb = At, bb_amt = Amt}) -> 
  game:broadcast(#notify_stage{game = Ctx#texas.gid, stage = ?GS_PREFLOP}, Ctx),
  ask(At, Ctx#texas{stage = ?GS_PREFLOP, max_betting = Amt});
start([Stage], Ctx = #texas{b = At}) -> 
  game:broadcast(#notify_stage{game = Ctx#texas.gid, stage = Stage}, Ctx),
  ask(At, Ctx#texas{stage = Stage, max_betting = 0}).

% not expectation seat player
betting(#cmd_raise{pid = PId}, Ctx = #texas{exp_seat = Exp}) when Exp#seat.pid /= PId -> 
  {continue, Ctx};

% betting timeout
betting({timeout, _, ?MODULE}, Ctx = #texas{exp_seat = Exp}) ->
  NotTimerCtx = cancel_timer(Ctx),
  betting(#cmd_fold{ pid = Exp#seat.pid }, NotTimerCtx);

%%% player call
betting(#cmd_raise{ amount = ?ZERO }, Ctx = #texas{exp_seat = Exp, exp_call = ?ZERO}) ->              % check
  NotTimerCtx = cancel_timer(Ctx),
  CheckedCtx = game:bet({Exp, ?ZERO}, NotTimerCtx),
  next_turn(Exp, CheckedCtx);
betting(#cmd_raise{ amount = ?ZERO }, Ctx = #texas{exp_seat = Exp, exp_call = ExpCall})               % poor all_in
when Exp#seat.inplay < ExpCall ->
  NotTimerCtx = cancel_timer(Ctx),
  PooredCtx = game:bet({Exp, Exp#seat.inplay}, NotTimerCtx),
  next_turn(Exp, PooredCtx);
betting(#cmd_raise{ amount = ?ZERO }, Ctx = #texas{exp_seat = Exp, exp_call = ExpCall}) ->            % call & check
  NotTimerCtx = cancel_timer(Ctx),
  CalledCtx = game:bet({Exp, ExpCall}, NotTimerCtx),
  next_turn(Exp, CalledCtx);

%%% player raise
betting(R = #cmd_raise{ amount = Raise}, Ctx = #texas{exp_min = Min, exp_max = Max}) when Raise < Min; Raise > Max  ->
  NotTimerCtx = cancel_timer(Ctx),
  betting(#cmd_fold{ pid = R#cmd_raise.pid }, NotTimerCtx);

betting(#cmd_raise{ amount = Raise}, Ctx = #texas{exp_seat = Exp, exp_call = Call}) -> % raise & all_in
  NotTimerCtx = cancel_timer(Ctx),
  BettedCtx = game:bet({Exp, Call, Raise}, NotTimerCtx),
  BettedSeats = seat:lookup(?PS_BET, BettedCtx#texas.seats),
  ResetedSeats = reset_seat(Exp#seat.sn, BettedSeats, BettedCtx#texas.seats),
  RaisedCtx = BettedCtx#texas{max_betting = BettedCtx#texas.max_betting + Call + Raise, seats = ResetedSeats},
  next_turn(Exp, RaisedCtx);

%%%
%%% fold
%%%

betting(#cmd_fold{pid = PId}, Ctx = #texas{exp_seat = Exp}) when Exp#seat.pid /= PId ->
  error_logger:info_report({cmd_fold, skip, other}),
  {skip, Ctx};

betting(#cmd_fold{}, Ctx = #texas{seats = S, exp_seat = Exp}) ->
  NotTimerCtx = cancel_timer(Ctx),
  FoldCtx = case Exp#seat.state =:= ?PS_LEAVE of
    true ->
      NotTimerCtx#texas{seats = seat:set(Exp#seat.sn, ?PS_LEAVE, S)};
    _ ->
      NotTimerCtx#texas{seats = seat:set(Exp#seat.sn, ?PS_FOLD, S)}
  end,

  game:broadcast(#notify_fold{game = Ctx#texas.gid, sn = Exp#seat.sn}, FoldCtx),
  next_turn(Exp, FoldCtx);

% skip
betting(_Msg, Ctx) ->
  {skip, Ctx}.

%%%
%%% private
%%%

ask(At = #seat{}, Ctx = #texas{seats = S}) ->
  ask(seat:lookup(?PS_PLAY, S, At), Ctx);
ask([_H], Ctx = #texas{}) ->
  {stop, Ctx};
ask([H|_], Ctx = #texas{}) ->
  ask_for_bet(H, Ctx).
  
% small blind fix big blind
ask_for_bet(H = #seat{inplay = Inplay, sn = SN, bet = B}, Ctx = #texas{sb = SB, stage = S})
when S =:= ?GS_PREFLOP, SN =:= SB#seat.sn, B =:= Ctx#texas.sb_amt ->
  ask_for_bet(H, Ctx, {Ctx#texas.max_betting, Inplay});
ask_for_bet(H = #seat{inplay = Inplay, sn = SN, bet = B}, Ctx = #texas{bb = BB, limit = Limit, stage = S})
when S =:= ?GS_PREFLOP, SN =:= BB#seat.sn, B =:= Ctx#texas.bb_amt ->
  ask_for_bet(H, Ctx, {Limit#limit.big, Inplay});
ask_for_bet(H = #seat{inplay = Inplay}, Ctx = #texas{}) ->
  ask_for_bet(H, Ctx, {Ctx#texas.max_betting - H#seat.bet , Inplay}).

ask_for_bet(H = #seat{}, Ctx = #texas{limit = Limit}, {?ZERO, Inplay}) ->
  ask_for_bet(H, Ctx, {Limit#limit.big, Inplay});
ask_for_bet(H = #seat{sn = SN, pid = PID}, Ctx = #texas{gid = Id}, {Min, Inplay}) ->
  ExpAmt = Ctx#texas.max_betting - H#seat.bet,
  Max = Inplay - ExpAmt,
  game:broadcast(#notify_actor{ game = Id, sn = SN }, Ctx),
  player:notify(H#seat.process, #notify_betting{ game = Id, player = PID, sn = SN, call = ExpAmt, min = Min, max = Max}),

  TimerCtx = start_timer(Ctx),
  ExpCtx = TimerCtx#texas{ exp_seat = H, exp_call = ExpAmt, exp_min = Min, exp_max = Max },

  {next, betting, ExpCtx}.
  
next_turn(At = #seat{}, Ctx = #texas{seats = S}) ->
  Active = seat:lookup(?PS_PLAY, S, At),
  Standing = seat:lookup(?PS_STANDING, S, At),
  next_turn(Standing, Active, Ctx).

next_turn(Standing, _, Ctx) when length(Standing) < 2 -> 
  game:broadcast(#notify_stage_end{game = Ctx#texas.gid, stage = Ctx#texas.stage}, Ctx),
  {goto, showdown, Ctx};
next_turn(_, [], Ctx = #texas{pot = Pot, seats = Seats}) ->
  NewPot = pot:new_stage(Pot),
  ResetSeats = reset_seat(seat:lookup(?PS_BET, Seats), Seats),
  game:broadcast(#notify_stage_end{game = Ctx#texas.gid, stage = Ctx#texas.stage}, Ctx),
  {stop, Ctx#texas{seats = ResetSeats, pot = NewPot}};
next_turn(_, [H|_], Ctx) -> 
  ask_for_bet(H, Ctx).

start_timer(Ctx = #texas{timeout = Timeout}) ->
  Timer = erlang:start_timer(Timeout, self(), ?MODULE),
  Ctx#texas{timer = Timer}.

cancel_timer(Ctx = #texas{timer = T}) ->
  catch erlang:cancel_timer(T),
  Ctx#texas{timer = ?UNDEF}.

reset_seat([], Seats) -> Seats;
reset_seat([H|T], Seats) ->
  reset_seat(T, seat:set(H#seat{state = ?PS_PLAY, bet = 0}, Seats)).

reset_seat(_SN, [], Seats) -> Seats;
reset_seat(SN, [#seat{sn = SN}|T], Seats) ->
  reset_seat(SN, T, Seats);
reset_seat(SN, [H = #seat{}|T], Seats) ->
  reset_seat(SN, T, seat:set(H#seat{state = ?PS_PLAY}, Seats)).
