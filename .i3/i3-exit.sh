#!/bin/bash

res=$(rofi -p '' -dmenu < ~/.i3/i3-exit-commands)

if [ "$res" == "logout" ]; then
    i3-msg exit
fi
if [ "$res" == "restart" ]; then
    reboot
fi
if [ "$res" == "shutdown" ]; then
    poweroff
fi
exit 0
