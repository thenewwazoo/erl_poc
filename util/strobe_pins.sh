#!/bin/bash

ecap_pin=$((0*32+7)) # GPIO0_7
control_pin=$((3*32+19)) #GPIO3_19
shared_pin=$((3*32+18)) #GPIO3_18

cd /sys/class/gpio


echo 0 >> gpio$control_pin/value
echo 1 >> gpio$control_pin/value
echo 0 >> gpio$control_pin/value
