% @doc GRiSP demonstration API.
-module(demo).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) ->
    {ok, Supervisor} = demo_sup:start_link(),
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    timer:sleep(5000),
    grisp_led:off(2),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    end,
    grisp_led:pattern(1, [{500, Random}]),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
