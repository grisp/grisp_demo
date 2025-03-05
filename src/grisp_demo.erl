% @doc GRiSP demonstration API.
-module(grisp_demo).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) ->
    DerCert = grisp_cryptoauth:read_cert(primary, der),
    PemCert = der_list_to_pem([DerCert]),
    ok = file:write_file("/etc/board.pem", PemCert),
    {ok, Supervisor} = grisp_demo_sup:start_link(),
    Colors = [{500, C} || C <- [magenta, white, aqua, yellow]],
    grisp_led:flash(2, red, 500),
    timer:sleep(5000),
    grisp_led:pattern(2, Colors),
    grisp_led:color(1, blue),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.

%--- Internal Functions --------------------------------------------------------

der_list_to_pem(DerCerts) when is_list(DerCerts) ->
    lists:map(fun der_to_pem_block/1, DerCerts).

der_to_pem_block(Der) when is_binary(Der) ->
    Enc64 = base64:encode(Der),
    Wrapped = wrap_base64(Enc64, 64),
    [ "-----BEGIN CERTIFICATE-----\n",
      Wrapped,
      "-----END CERTIFICATE-----\n" ].

wrap_base64(Base64, LineLen) ->
    wrap_lines(Base64, LineLen, []).

wrap_lines(<<>>, _LineLen, Acc) ->
    lists:reverse(Acc);
wrap_lines(Data, LineLen, Acc) ->
    case Data of
        <<Line:LineLen/binary, Rest/binary>> ->
            wrap_lines(Rest, LineLen, ["\n", Line | Acc]);
        LastLine ->
            wrap_lines(<<>>, LineLen, ["\n", LastLine | Acc])
    end.
