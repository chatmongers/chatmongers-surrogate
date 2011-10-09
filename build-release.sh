#!/bin/bash

pushd extras
if [ ! -d exmpp-0.9.8 ]; then
./build-exmpp.sh
fi
popd

./rebar get-deps
./rebar update-deps
./rebar clean
./rebar compile
cp -r ebin include deps/chatmonger/

./rebar generate force=1
