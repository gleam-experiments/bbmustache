-module(gleam_bbmustache_native).

-export([try_catch/1]).

try_catch(F) ->
  try {ok, F()}
  catch Error -> {error, Error}
  end.
