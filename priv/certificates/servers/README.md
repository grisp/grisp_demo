# Development Enviroment CA

This information is for internal use, ignore this directory if you are not a 
developer.


## Optional server chain

`grisp_demo` searches this directories for certificates that have the same name
of the domain it is tryng to connect to.

When you are developing the backend for this client you can use this directory
to place the self-signed CA certificate you are using for your backend instance.

The filename of the certificate must match the domain name you are using in the 
`domain` setting, followed by the `".pem"` termination.

To create the backend server chain for development, look into the private 
internal documentation.

Never commit a certificate in this directory. This directory should not be part 
of hex.pm releases.
