
A Very Bad Idea
===============

Simply put, what I'm attempting to do here is a bad idea, but I can't help myself.

Why
---

There is no such thing as a general-purpose toothed-wheel position sensor on the market. There are special code paths for every type of wheel, and there are many types of wheels. 60-1, 36-2-2-2, 12-3, 4-1, etc. There are even wheels that don't have teeth, but instead shine LEDs through windows with varying angular durations. To use them requires careful timing and inference.

Using [Markov localization](http://correll.cs.colorado.edu/?p=1319), we can deduce where we are, allowing for some uncertainty, without having to develop specialized routines for each possible type of position sensor. We simply build a 1-D "world", presume that world is finite and looped, and localize ourselves within it. The rest is just table lookups and twiddling bits.

What
----

This software is loosely targetted at the BeagleBone Black. Because reasons, it's tightly coupled to TI's EZ-SDK v6 on that platform, and thus Linux 3.2.0. Boo-hiss. Call TI and tell them to figure out why my kernel module doesn't work on a properly modern version.

I haven't successfully gotten Erlang to cross-compile, and doing so sucks anyway, so I'm using the Raspbian distribution. Because I suspect that build doesn't do proper hardware floating-point math, I'll need to fix that eventually. Due to the vagaries of cross-compiling Erlang, I've had to write an Erlang C node to watch the kernel attributes my module exposes.

Also, Erlang is kind of neat.

When
----

When I get the time.

How
---

The kernel module initializes the eCAP module, which starts a timer. When the eCAP pin goes high, the module stores the current timestamp in a register, and interrupts the kernel. The kernel module then unblocks any poll(3)s of the .../capture attribute. The Erlang C node ecap-node then reads in the capture attribute, as well as the capture registers. It passes those along to the Erlang VM in the form of messages.

From there, data is analyzed and messages passed along. The overriding design principle is intended to be one of "subscription" to events. If you need to know about an event that a process announces, you subscribe yourself to its event manager. From there, events can propagate through the system, triggering recalculations as necessary. For example, a tachometer module could subscribe itself to the localization engine and receive TDC events. 


