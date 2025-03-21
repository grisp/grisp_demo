# grisp_demo

Demo application for GRiSP boards.
It includes grisp_connect to interface with grisp.io services,
and grisp_updater to enable software updates.


## Generate GRiSP Firmwares

To generate the firmware image to write on the GRiSP board EMMC, run:

	rebar3 as dev grisp firmware -i -r -f

or:

	rebar3 as prod grisp firmware -i -r -f

This generate the system firmware to update a single system partition,
and the full EMMC image to reset the GRiSP board EMMC.
See [rebar3_grisp documentation](https://hexdocs.pm/rebar3_grisp/readme.html#generate-grisp-2-firmwares)
for more information on how to write firmwares.


## Generate a software update package

To create a software update package, run:

	$ rebar3 as dev grisp pack -r -f

or:

	$ rebar3 as prod grisp pack -r -f

This will generate an unsigned package, for the package to be verified you need to:

 - Change the configuration key `signature_check` to `true`.
 - Add the public key into `priv/certificates/updates`.
 - Generate the package with either:

 	```
	$ rebar3 as prod grisp pack -r -f -k private_key.pem
	$ rebar3 as dev grisp pack -r -f -k private_key.pem
	```

Note that updates will be verified only after this change itself is installed
on the board.

See [rebar3_grisp documentation](https://hexdocs.pm/rebar3_grisp/readme.html#build-software-update-package)
for more information on how to update.


## Development

Add your development server certificate if it is self-signed, or the
development CA certificate used to sign the server certificate into
`priv/certificates/servers`.

e.g.

	ln -s $MY_SERVER_REPO/priv/_dev_certs/www.seawater.local.CA.pem priv/certificates/servers/www.seawater.local.pem


### Local Development

The local development release runs on the host itself and connects to a local server.

Add an entry in your local hosts file so the domain www.seawater.local points
to your local development server. Remember to configure the local server to
check the client certificate against the `grisp_connect` test CA.

For the default configuration to work, the grisp_connect dependency must be
in your `_checkouts` directory.

Start a local development shell:

    rebar3 as local shell

Run tests:

    rebar3 ct

To start a local development shell with support for TLS distribution, you need 
first to generate a testing CA and certificate:

	local/setup.sh

Then you can start the shell with:

	ERL_FLAGS='-proto_dist inet_tls -ssl_dist_optfile local/ssl_dist_opts.rel -connect_all false' rebar3 as local shell --sname local --setcookie grisp


### Development on GRiSP Hardware

The development release runs on a GRiSP board and connects to a local server.

Add an entry in the grisp hosts file so the domain www.seawater.local points
to your local development server.

e.g. using `ifconfig` command (MacOS and older linux):

	mkdir -p grisp/grisp2/common/deploy/files/etc
    echo "$(ifconfig | grep 'inet ' | grep -v 127.0.0.1 | awk '{ print $2 }' | head -n 1) www.seawater.local" >> grisp/grisp2/common/deploy/files/etc/hosts

e.g. using `ip` command (Newer linux):

	mkdir -p grisp/grisp2/common/deploy/files/etc
    echo "$(ip addr show | grep 'inet ' | grep -v 127.0.0.1 | awk '{print $2}' | cut -d/ -f1 | head -n 1) www.seawater.local" >> grisp/grisp2/common/deploy/files/etc/hosts

The deploy the release, configure rebar3_grisp's deploy configuration in
rebar.config and then run:

    rebar3 as dev grisp deploy

To generate the firmwares:

	rebar3 as dev grisp firmware -i -r -f

To create a software update package:

	rebar3 as dev grisp pack -r -f


## Production

The production release runs on a GRiSP board and connect to a production server.

To deploy on GRiSP hardware for production, configure rebar3_grisp's deploy
configuration in rebar.config and then run:

    rebar3 as prod grisp deploy

To generate the firmwares:

	rebar3 as prod grisp firmware -i -r -f

To create a software update package:

	rebar3 as prod grisp pack -r -f

To deploy to grisp.io:

	rebar3 grisp-io auth
	rebar3 as prod grisp-io upload -r -f


## Known Issues

### Issues with hostname resolution (nxdomain)

For proper DNS resolution using DHCP-given DNS, when building from a pre-built
OTP package, the minimum OTP version MUST be 27.2.4.
If you still have issues, you may have an old version of the pre-built package.
Try cleaning up the pre-built package cache.

On MacOS:

	rm ~/Library/Caches/grisp/packages/grisp2/otp/*
