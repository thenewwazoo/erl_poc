#!/bin/bash

ecap_pin=$((0*32+7)) # GPIO0_7
control_pin=$((3*32+19)) #GPIO3_19
shared_pin=$((3*32+18)) #GPIO3_18

cd /sys/class/gpio

echo $shared_pin >> export
echo in >> gpio$shared_pin/direction

echo $control_pin >> export
echo out >> gpio$control_pin/direction

#echo $control_pin >> unexport
#echo $shared_pin >> unexport
