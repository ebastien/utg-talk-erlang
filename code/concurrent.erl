-module(concurrent).
-compile(export_all).

loop() ->
  receive
    hello ->
      io:format("Unleash the geeks!~n");
    _ ->
      loop()
  end.
