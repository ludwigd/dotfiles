#!/bin/bash

# rofi-mode-sway - Switch windows under Sway using Rofi
# Copyright (C) 2023 Damian Ludwig
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

arg=$1
if [ -z "$arg" ]; then
    swaymsg -t get_tree |
        jq -r '.nodes[].nodes[] | if .nodes then [recurse(.nodes[])] else [] end +
	   .floating_nodes | .[] | select(.nodes==[]) |
	   if .shell=="xwayland"
	   then
		(.name + "\\0info\\x1f" + (.id | tostring) + "\\x1fmeta\\x1f" + .window_properties.class)
	   else
		(.name + "\\0info\\x1f" + (.id | tostring) + "\\x1fmeta\\x1f" + .app_id)
	   end' |
	xargs -0 printf "%b" 
else
    swaymsg "[con_id=$ROFI_INFO]" focus > /dev/null
fi 
