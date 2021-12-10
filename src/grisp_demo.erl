% @doc GRiSP demonstration API.
-module(grisp_demo).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) ->
    {ok, Supervisor} = grisp_demo_sup:start_link(),
    grisp_led:flash(2, red, 500),
    timer:sleep(5000),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    end,
    grisp_led:pattern(2, [{500, Random}]),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
