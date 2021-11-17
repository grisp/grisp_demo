% @doc GRiSP demonstration API.
-module(demo).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> demo_sup:start_link().

% @private
stop(_State) -> ok.
