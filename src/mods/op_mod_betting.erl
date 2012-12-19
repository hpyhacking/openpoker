-module(op_mod_betting).
-behaviour(op_exch_mod).
-export([start/2, dispatch/2, betting/2]).

-include("openpoker.hrl").

-define(Z, 0).

%%%
%%% callback
%%%

start([?GS_PREFLOP], Ctx = #texas{bb = At}) -> 
  game:broadcast(#notify_stage{game = Ctx#texas.gid, stage = ?GS_PREFLOP}, Ctx),
  ask(At, Ctx#texas{stage = ?GS_PREFLOP, max_betting = Ctx#texas.bb_amt});

start([Stage], Ctx = #texas{b = At}) -> 
  game:broadcast(#notify_stage{game = Ctx#texas.gid, stage = Stage}, Ctx),
  ask(At, Ctx#texas{stage = Stage, max_betting = 0}).

dispatch(_R, _Ctx) ->
  ok.

betting({timeout, _, ?MODULE}, Ctx = #texas{exp_seat = Exp}) ->
  NotTimerCtx = cancel_timer(Ctx),
  betting(#cmd_fold{ pid = Exp#seat.pid }, NotTimerCtx);

%%%
%%% cmd_raise
%%%

% not expectation seat player
betting(#cmd_raise{pid = ExpPId}, Ctx = #texas{exp_seat = Exp})
when ExpPId /= Exp#seat.pid -> 
  {continue, Ctx};

%%% all-in - less call
betting(#cmd_raise{ amount = ?Z }, Ctx = #texas{exp_seat = Exp, exp_call = ?Z, exp_min = ?Z, exp_max = ?Z}) ->
  NotTimerCtx = cancel_timer(Ctx),
  PooredCtx = game:bet({Exp, Exp#seat.inplay}, NotTimerCtx),
  next_turn(Exp, PooredCtx);

%%% check or call
betting(#cmd_raise{ amount = ?Z }, Ctx = #texas{exp_seat = Exp, exp_call = ExpCall}) ->
  NotTimerCtx = cancel_timer(Ctx),
  CheckedCtx = game:bet({Exp, ExpCall}, NotTimerCtx),
  next_turn(Exp, CheckedCtx);

%%% fold other zero cmd_raise
betting(#cmd_raise{ pid = Pid, amount = ?Z }, Ctx) ->
  NotTimerCtx = cancel_timer(Ctx),
  betting(#cmd_fold{ pid = Pid }, NotTimerCtx);

%%% raise to all-in
betting(R = #cmd_raise{ amount = Max }, Ctx = #texas{exp_min = ?Z, exp_max = Max}) -> 
  raise(R, Ctx);

%%% normal raise, perhaps to all-in
betting(R = #cmd_raise{ amount = Raise }, Ctx = #texas{exp_min = Min, exp_max = Max})
when Min /= ?Z, Raise >= Min, Raise =< Max ->
  raise(R, Ctx);

betting(#cmd_raise{pid = Pid}, Ctx) ->
  NotTimerCtx = cancel_timer(Ctx),
  betting(#cmd_fold{ pid = Pid }, NotTimerCtx);

%%%
%%% fold
%%%

betting(#cmd_fold{pid = PId}, Ctx = #texas{exp_seat = Exp})
when PId /= Exp#seat.pid -> 
  {continue, Ctx};

betting(#cmd_fold{}, Ctx = #texas{seats = S, exp_seat = Exp}) ->
  NotTimerCtx = cancel_timer(Ctx),
  FoldCtx = NotTimerCtx#texas{seats = seat:set(Exp#seat.sn, ?PS_FOLD, S)},
  game:broadcast(#notify_fold{game = Ctx#texas.gid, sn = Exp#seat.sn}, FoldCtx),
  next_turn(Exp, FoldCtx);

%%%
%%% leave
%%%

betting(R = #cmd_leave{pid = PId}, Ctx = #texas{seats = S, exp_seat = Exp})
when PId =:= Exp#seat.pid ->
  NotTimerCtx = cancel_timer(Ctx),
  FoldCtx = NotTimerCtx#texas{seats = seat:set(Exp#seat.sn, ?PS_FOLD, S)},
  game:broadcast(#notify_fold{game = Ctx#texas.gid, sn = Exp#seat.sn}, FoldCtx),
  LeavedCtx = game:do_leave(R, FoldCtx),
  next_turn(Exp, LeavedCtx);

% skip
betting(_Msg, Ctx) ->
  {skip, Ctx}.

%%%
%%% private
%%%

raise(#cmd_raise{amount = R}, Ctx = #texas{exp_seat = Exp}) ->
  NotTimerCtx = cancel_timer(Ctx),
  BettedCtx = game:bet({Exp, Ctx#texas.exp_call, R}, NotTimerCtx),
  BettedSeats = seat:lookup(?PS_BET, BettedCtx#texas.seats),
  ResetedSeats = reset_seat(Exp#seat.sn, BettedSeats, BettedCtx#texas.seats),
  next_turn(Exp, BettedCtx#texas{
      max_betting = BettedCtx#texas.max_betting + R, 
      seats = ResetedSeats}).

ask(At = #seat{}, Ctx = #texas{seats = S}) ->
  ask(seat:lookup(?PS_PLAY, S, At), Ctx);
ask([], Ctx = #texas{}) ->
  {stop, Ctx};
ask([_H], Ctx = #texas{}) ->
  {stop, Ctx};
ask([H|_], Ctx = #texas{}) ->
  ask_for_bet(H, Ctx).
  

get_min_raise(BB, _TurnMaxBet = ?Z) -> BB;
get_min_raise(_BB, TurnMaxBet) -> TurnMaxBet.
  
ask_for_bet(H = #seat{sn = SN, pid = PID, inplay = Inplay}, Ctx = #texas{gid = Id, max_betting = TurnMaxBet}) ->
  ExpCall = TurnMaxBet - H#seat.bet,
  MinRaise = get_min_raise(Ctx#texas.bb_amt, TurnMaxBet),

  [Call, Min, Max] = case Inplay of
    Inplay when Inplay < ExpCall ->
      [?Z, ?Z, ?Z]; %% ONLY All-in
    Inplay -> 
      case Inplay - ExpCall of
        R when R < MinRaise ->
          [ExpCall, ?Z, R]; %% Call or All-in, Don't Raise
        R when R >= MinRaise ->
          [ExpCall, MinRaise, R] %% Call or Raise or All-in
      end
  end,

  game:broadcast(#notify_actor{ game = Id, player = PID, sn = SN, timeout = Ctx#texas.timeout }, Ctx),
  player:notify(H#seat.process, #notify_betting{ game = Id, player = PID, sn = SN, call = Call, min = Min, max = Max}),
  TimerCtx = start_timer(Ctx),
  ExpCtx = TimerCtx#texas{ exp_seat = H, exp_call = Call, exp_min = Min, exp_max = Max },
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

start_timer(Ctx) ->
  game:start_timer(Ctx, ?MODULE).

cancel_timer(Ctx) ->
  game:cancel_timer(Ctx).

reset_seat([], Seats) -> Seats;
reset_seat([H|T], Seats) ->
  reset_seat(T, seat:set(H#seat{state = ?PS_PLAY, bet = ?Z}, Seats)).

reset_seat(_SN, [], Seats) -> Seats;
reset_seat(SN, [#seat{sn = SN}|T], Seats) ->
  reset_seat(SN, T, Seats);
reset_seat(SN, [H = #seat{}|T], Seats) ->
  reset_seat(SN, T, seat:set(H#seat{state = ?PS_PLAY}, Seats)).
