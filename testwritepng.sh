#!/bin/sh

#QUERY_STRING="w=160&h=120&f={'t':'y'}" ./writepng > testc.png
#QUERY_STRING="w=160&h=120&f={'t':'x'}" ./writepng > testc.png
#QUERY_STRING="w=160&h=120&f={'t':'atan','m':'c','ch':[{'t':'x'}]}" ./writepng > testc.png
#QUERY_STRING="w=160&h=120&f={'t':'add','m':'w','ch':[{'t':'x'}, {'t':'y'}]}" ./writepng > testc.png
#QUERY_STRING="w=140&h=140&f={'t':'cchsl','ch':[{'t':'num','val':1.0}, {'t':'x'},{'t':'y'}]}" ./writepng > testc.png
#QUERY_STRING="w=140&h=140&f={'t':'ccrgb','ch':[{'t':'num','val':1.0}, {'t':'x'},{'t':'y'}]}" ./writepng > testc.png
#QUERY_STRING="w=140&h=140&f={'t':'num','val':1.0}" gdb ./writepng > testc.png
#QUERY_STRING="w=140&h=140&f={'t':'ccrgb','ch':[{'t':'add','m':'c','ch':[{'t':'neg','ch':[{'t':'rd','ch':[{'t':'y'}]}]},{'t':'mul','m':'w','ch':[{'t':'sub','m':'w','ch':[{'t':'rd','ch':[{'t':'ru','ch':[{'t':'num','val':0.3494903080882794}]}]},{'t':'x'}]},{'t':'atan','m':'c','ch':[{'t':'exp','m':'w','ch':[{'t':'num','val':0.6482117126567887}]}]}]}]},{'t':'add','m':'c','ch':[{'t':'y'},{'t':'log','m':'c','ch':[{'t':'sin','ch':[{'t':'ccrgb','ch':[{'t':'div','m':'w','ch':[{'t':'sub','m':'c','ch':[{'t':'add','m':'w','ch':[{'t':'num','val':0.5232982069197148},{'t':'y'}]},{'t':'y'}]},{'t':'x'}]},{'t':'x'},{'t':'num','val':0.5785528617383234}]}]}]}]},{'t':'x'}]}" ./writepng > testc.png
QUERY_STRING="w=160&h=120&f={'t':'bwperlin','seed':1432123,'ch':[{'t':'x'},{'t':'y'}]}" ./writepng > testc.png
