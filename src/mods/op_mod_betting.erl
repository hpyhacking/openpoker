-module(op_mod_betting).
-behaviour(op_exch_mod).
-export([start/2, dispatch/2, betting/2]).

-include("openpoker.hrl").

-define(ZERO, 0).

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
betting(#cmd_raise{pid = PId}, Ctx = #texas{exp_seat = Exp}) when Exp#seat.pid /= PId -> 
  {continue, Ctx};

%%% check
betting(#cmd_raise{ amount = ?ZERO }, Ctx = #texas{exp_seat = Exp, exp_call = ?ZERO}) ->
  NotTimerCtx = cancel_timer(Ctx),
  CheckedCtx = game:bet({Exp, ?ZERO}, NotTimerCtx),
  next_turn(Exp, CheckedCtx);

%%% all-in
betting(#cmd_raise{ amount = ?ZERO }, Ctx = #texas{exp_seat = Exp, exp_call = ExpCall})
when Exp#seat.inplay < ExpCall ->
  NotTimerCtx = cancel_timer(Ctx),
  PooredCtx = game:bet({Exp, Exp#seat.inplay}, NotTimerCtx),
  next_turn(Exp, PooredCtx);

%%% call
betting(#cmd_raise{ amount = ?ZERO }, Ctx = #texas{exp_seat = Exp, exp_call = ExpCall}) ->
  NotTimerCtx = cancel_timer(Ctx),
  CalledCtx = game:bet({Exp, ExpCall}, NotTimerCtx),
  next_turn(Exp, CalledCtx);

%%% error cmd_raise
betting(#cmd_raise{ pid = Pid, amount = ?ZERO }, Ctx) ->
  NotTimerCtx = cancel_timer(Ctx),
  betting(#cmd_fold{ pid = Pid }, NotTimerCtx);

%%% raise or all-in
betting(#cmd_raise{ amount = Raise}, Ctx = #texas{exp_seat = Exp, exp_call = Call}) -> 
  NotTimerCtx = cancel_timer(Ctx),
  BettedCtx = game:bet({Exp, Call, Raise}, NotTimerCtx),
  BettedSeats = seat:lookup(?PS_BET, BettedCtx#texas.seats),
  ResetedSeats = reset_seat(Exp#seat.sn, BettedSeats, BettedCtx#texas.seats),
  next_turn(Exp, BettedCtx#texas{
      max_betting = BettedCtx#texas.max_betting + Raise, 
      seats = ResetedSeats});

%%%
%%% fold
%%%

betting(#cmd_fold{pid = PId}, Ctx = #texas{seats = S, exp_seat = Exp}) when Exp#seat.pid =:= PId ->
  NotTimerCtx = cancel_timer(Ctx),
  FoldCtx = NotTimerCtx#texas{seats = seat:set(Exp#seat.sn, ?PS_FOLD, S)},
  game:broadcast(#notify_fold{game = Ctx#texas.gid, sn = Exp#seat.sn}, FoldCtx),
  next_turn(Exp, FoldCtx);

betting(#cmd_fold{}, Ctx) ->
  {continue, Ctx};

%%%
%%% leave
%%%

betting(R = #cmd_leave{pid = PId}, Ctx = #texas{seats = S, exp_seat = Exp}) when Exp#seat.pid =:= PId ->
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

ask(At = #seat{}, Ctx = #texas{seats = S}) ->
  ask(seat:lookup(?PS_PLAY, S, At), Ctx);
ask([], Ctx = #texas{}) ->
  {stop, Ctx};
ask([_H], Ctx = #texas{}) ->
  {stop, Ctx};
ask([H|_], Ctx = #texas{}) ->
  ask_for_bet(H, Ctx).
  
ask_for_bet(H = #seat{sn = SN, pid = PID, inplay = Inplay}, Ctx = #texas{gid = Id, max_betting = MaxBet}) ->
  Call = MaxBet - H#seat.bet,
  %% 当前最大下注额如果为0
  %% 则加注的最小额为大盲
  Min = case MaxBet of
    0 ->
      Ctx#texas.bb_amt;
    _ ->
      MaxBet
  end,
  %% 如果玩家筹码可以跟注
  %% 则最大加注额为下注后的剩余筹码
  Max = case Inplay of
    Inplay when Inplay >= Call ->
      Inplay - Call;
    Inplay ->
      Inplay
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
  reset_seat(T, seat:set(H#seat{state = ?PS_PLAY, bet = 0}, Seats)).

reset_seat(_SN, [], Seats) -> Seats;
reset_seat(SN, [#seat{sn = SN}|T], Seats) ->
  reset_seat(SN, T, Seats);
reset_seat(SN, [H = #seat{}|T], Seats) ->
  reset_seat(SN, T, seat:set(H#seat{state = ?PS_PLAY}, Seats)).
