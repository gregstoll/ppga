#!/bin/sh

#QUERY_STRING="w=160&h=120&f={'t':'y'}" ./writepng > testc.png
QUERY_STRING="w=160&h=120&f={'t':'x'}" ./writepng > testc.png
#QUERY_STRING="w=160&h=120&f={'t':'atan','m':'c','ch':[{'t':'x'}]}" ./writepng > testc.png
