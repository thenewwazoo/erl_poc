CC=/home/bmatt/Development/BeagleBoneBlack/ti_ezsdk/ti-sdk-am335x-evm-07.00.00.00/linux-devkit/sysroots/i686-arago-linux/usr/bin/arm-linux-gnueabihf-gcc
LIBS="-lerl_interface -lei -pthread"
INCLUDE=-I/home/bmatt/Development/BeagleBoneBlack/erlang/esl-erlang/root/usr/lib/erlang/lib/erl_interface-3.7.9/include/ 
LIBINC=-L/home/bmatt/Development/BeagleBoneBlack/erlang/esl-erlang/root/usr/lib/erlang/lib/erl_interface-3.7.9/lib/
DEBUG=-DDEBUG

$CC -o ecap_node ecap_node.c $LIBS $INCLUDE $LIBINC $DEBUG
