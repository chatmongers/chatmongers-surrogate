#!/bin/bash

function check_deps {
	rpm -qa $1 |grep $1
	if [ $? -ne 0 ]; then
		yum -y install $1
	fi
}

tar zxf exmpp-0.9.8.tar.gz

cd exmpp-0.9.8

check_deps autoconf
check_deps automake
check_deps expat-devel
check_deps gcc
check_deps libtool
check_deps openssl-devel
check_deps zlib-devel
check_deps make

autoreconf -i
./configure
make
make install


