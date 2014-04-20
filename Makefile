
#call make as such:
#
#$ make ARCH=arm CROSS_COMPILE=arm-linux-gnueabihf-
#

obj-m += gpiohw.o ecap_plat.o

#INCLUDE += -I/home/bmatt/Development/BeagleBoneBlack/kernel/kernel/arch/arm/plat-omap/include
#XENODIR = /home/bmatt/Development/BeagleBoneBlack/nerves/nerves-sdk/buildroot/output/host/usr/arm-buildroot-linux-gnueabihf/sysroot/usr
#INCLUDE += -I$(XENODIR)/include/xenomai
CCFLAGS=-march=armv7-a -mtune=cortex-a8 -mfpu=neon -ftree-vectorize -mfloat-abi=softfp
#XENO_CFLAGS=-D_GNU_SOURCE -D_REENTRANT -D__XENO__
#override ccflags-y=$(CCFLAGS) $(XENO_CFLAGS) $(INCLUDE)
#LIBDIRS=-L/home/bmatt/Development/BeagleBoneBlack/nerves/nerves-sdk/buildroot/output/host/usr/arm-buildroot-linux-gnueabihf/sysroot/usr/lib/arm-linux-gnueabi #-L$(XENODIR)/lib
#LIBS=-lpthread -lrt# -lxenomai
#override LDFLAGS=$(LIBDIRS) $(LIBS)
#BUILDDIR = /home/bmatt/Development/BeagleBoneBlack/vanilla_kernel/linux-dev/KERNEL
#BUILDDIR=/home/bmatt/Development/BeagleBoneBlack/vanilla_kernel/no_xenomai/kernel/kernel
BUILDDIR=/home/bmatt/Development/BeagleBoneBlack/ti_ezsdk/ti-sdk-am335x-evm-06.00.00.00/board-support/linux-3.2.0-psp04.06.00.11/
#BUILDDIR=/home/bmatt/Development/BeagleBoneBlack/ti_ezsdk/ti-sdk-am335x-evm-07.00.00.00/board-support/linux-3.12.10-ti2013.12.01/
#BUILDDIR=/home/bmatt/Development/BeagleBoneBlack/vanilla_kernel/beaglebone_3.12/kernel

all:
	make -C $(BUILDDIR) SUBDIRS=$(PWD) modules

clean:
	make -C $(BUILDDIR) M=$(PWD) clean
