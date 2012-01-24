#!/bin/sh

umask 7000

mkdir "/var/lib/hsconvergence"
openssl genrsa -out "/var/lib/hsconvergence/privkey.pem" 2048
touch "/var/lib/hsconvergence/fingerprints.sqlite"
