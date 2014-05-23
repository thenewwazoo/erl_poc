
#call make as such:
#
#$ make ARCH=arm CROSS_COMPILE=arm-linux-gnueabihf-
#

obj-m += c_src/ti-ecap-capture.o

CCFLAGS=-march=armv7-a -mtune=cortex-a8 -mfpu=neon -ftree-vectorize -mfloat-abi=softfp
BUILDDIR=/home/bmatt/Development/BeagleBoneBlack/ti_ezsdk/ti-sdk-am335x-evm-06.00.00.00/board-support/linux-3.2.0-psp04.06.00.11/

all:
	make -C $(BUILDDIR) SUBDIRS=$(PWD) modules

clean:
	make -C $(BUILDDIR) M=$(PWD) clean
