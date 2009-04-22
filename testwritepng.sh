#!/bin/sh

#QUERY_STRING="w=160&h=120&f={'t':'y'}" ./writepng > testc.png
#QUERY_STRING="w=160&h=120&f={'t':'x'}" ./writepng > testc.png
#QUERY_STRING="w=160&h=120&f={'t':'atan','m':'c','ch':[{'t':'x'}]}" ./writepng > testc.png
#QUERY_STRING="w=160&h=120&f={'t':'add','m':'w','ch':[{'t':'x'}, {'t':'y'}]}" ./writepng > testc.png
QUERY_STRING="w=140&h=140&f={'t':'cchsl','ch':[{'t':'num','val':1.0}, {'t':'num','val':0.0},{'t':'num','val':0.0}]}" ./writepng > testc.png
#QUERY_STRING="w=140&h=140&f={'t':'num','val':1.0}" ./writepng > testc.png
