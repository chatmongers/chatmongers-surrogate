#!/bin/bash

pkglist=`mktemp -u`
pkginstall_list=`mktemp -u`

rm -rf $pkginstall_list

function check_deps {
    if [ ! -f "$pkglist" ]; then
        echo "Refresh packages..."
        dpkg -l |grep "^ii" | awk '{print $2 }' > $pkglist
    fi
    grep "^$1\$" $pkglist
    if [ $? -ne 0 ]; then
        echo "Package $1 not installed.  Installing..."
        echo $1 >> $pkginstall_list
    else
        echo "Package $1 is installed..."
    fi
}

check_deps erlang
check_deps autoconf
check_deps automake
check_deps libexpat1-dev
check_deps build-essential
check_deps libtool
check_deps openssl
check_deps libssl-dev
check_deps make
check_deps git

if [ -f $pkginstall_list ]; then
    apt-get -y install `cat $pkginstall_list | xargs`
    rm $pkginstall_list
fi

rm $pkglist

if [ ! -d exmpp-0.9.8 ]; then
	tar zxf exmpp-0.9.8.tar.gz

	cd exmpp-0.9.8
	autoreconf -i
	./configure
	make
	sudo make install
fi

