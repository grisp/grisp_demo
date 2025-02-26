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
    Colors = [{500, C} || C <- [magenta, white, aqua, yellow]],
    grisp_led:flash(2, red, 500),
    timer:sleep(5000),
    grisp_led:pattern(2, Colors),
    grisp_led:color(1, blue),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
