#!/bin/env python3

import os
import time
import psutil

# battery critical threshold
threshold = 20

while True:
        # get the battery
        bat = psutil.sensors_battery()

        # action
        if not bat.power_plugged:
                if bat.percent < threshold:
                        os.system("notify-send -w -u critical 'Battery low'")

        # wait for next cycle
        time.sleep(10)
