#!/bin/vsh

pkill polybar

sleep 1;

polybar --reload i3bar &
