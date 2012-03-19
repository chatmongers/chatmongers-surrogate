#!/bin/bash

pushd extras
./build-exmpp.sh
popd

./rebar get-deps
./rebar update-deps
./rebar clean
./rebar compile
cp -r ebin include deps/chatmonger/

./rebar generate force=1
