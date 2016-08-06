#!/bin/bash

declare -i AreaBottomEdge
AreaBottomEdge=`synclient -l | sed -n 's/[ \t]*AreaBottomEdge[ \t]*= //p'`
declare -i AreaTopEdge
AreaTopEdge=`synclient -l | sed -n 's/[ \t]*AreaTopEdge[ \t]*= //p'`
if [ $AreaBottomEdge -eq 1 ] && [ $AreaTopEdge -eq 0 ]
then
    synclient AreaBottomEdge=0
    synclient AreaTopEdge=2500
    echo "Touchpad enabled"
else
    synclient AreaTopEdge=0
    synclient AreaBottomEdge=1
    echo "Touchpad disabled"
fi
