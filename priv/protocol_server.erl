#!/usr/bin/env escript

-export([main/1]).

main(_) ->
  code:add_patha("ebin"),
  code:add_patha("deps/mochiweb/ebin"),
  code:add_patha("deps/webtekcos/ebin"),

  Loop = fun () ->
      receive 
        _ ->
          ok
      end
  end,

  % mock player id, beascuse protocol check player id to pair pid.
  MockPlayerPID = spawn(Loop),
  yes = global:register_name({player, 1}, MockPlayerPID),

  webtekcos:start_link("127.0.0.1", 8000),

  io:format("Open client.html test websocket server.~n"),
  io:format("It's listen on localhost:8000.~n"),
  io:format("Press Ctrl+C to shutdown server!!!~n"),

  receive
    _ -> ok
  end.
