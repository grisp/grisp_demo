#!/bin/bash

set -e 

usage() {
    local code="$1"
    >&2 echo "usage: $0"
    exit $code
}

error() {
    local code="$1"
    local msg="$2"
    >&2 echo "error: $msg"
    usage $code
}

ROOT="$(cd $(dirname "$0"); pwd)"
HOSTNAME="$(hostname -s)"
PEERS_CA_CHAIN="${ROOT}/peers.CA.pem"

for file in "local.CA.cfg" "local_v3_ext.cfg" "local.cfg" "ssl_dist_opts.rel"; do
    eval "echo \"$(< ${ROOT}/${file}.template)\"" > "${ROOT}/${file}"
done

openssl genrsa -out "${ROOT}/local.CA.key" 4096
openssl req -new -x509 -config "${ROOT}/local.CA.cfg" -key "${ROOT}/local.CA.key" -out "${ROOT}/local.CA.pem" -days 36500
openssl genrsa -out "${ROOT}/local.key" 2048
openssl req -new -config "${ROOT}/local.cfg" -key "${ROOT}/local.key" -out "${ROOT}/local.csr"
openssl x509 -req -extfile "${ROOT}/local_v3_ext.cfg" \
        -in "${ROOT}/local.csr" -CA "${ROOT}/local.CA.pem" -CAkey "${ROOT}/local.CA.key" \
        -CAcreateserial -out "${ROOT}/local.pem" -days 36500
openssl verify -CAfile "${ROOT}/local.CA.pem" "${ROOT}/local.pem" || error $? "Failed to verify generated certificate"
