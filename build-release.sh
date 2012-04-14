#!/bin/bash

tarfile=Mnesia-`date "+%Y%m%d%H%M%S"`.tar
if [ -d rel/surrogate ]; then
	tar cf $tarfile rel/surrogate/Mnesia*

	echo Backup mnesia to $tarfile
	tar tf $tarfile
fi

pushd extras
./build-exmpp.sh
popd

./rebar get-deps
./rebar update-deps
./rebar clean
./rebar compile
cp -r ebin include deps/chatmonger/

./rebar generate force=1

if [ -f $tarfile ]; then
	tar xf $tarfile
fi

