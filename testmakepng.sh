#!/bin/sh

#./makepng.py 'width=160&height=120&func={"t":"num","val":0.3}'
#./makepng.py "width=160&height=120&func={'t':'atan','m':'c','ch':[{'t':'x'}]}"
#./makepng.py "width=160&height=120&func={'t':'add','m':'c','ch':[{'t':'x'},{'t':'atan','m':'w','ch':[{'t':'y'}]}]}"
#./makepng.py "width=160&height=120&func={'t':'add','m':'c','ch':[{'t':'x'},{'t':'atan','m':'w','ch':[{'t':'y'}]}]}"
#./makepng.py "width=140&height=140&func={'t':'atan','m':'w','ch':[{'t':'log','m':'w','ch':[{'t':'ru','ch':[{'t':'x'}]}]}]}"
#./makepng.py "w=140&h=140&f={'t':'mul','m':'w','ch':{'0':{'t':'sub','m':'c','ch':{'0':{'t':'cos','ch':{'0':{'t':'y'}}},'1':{'t':'add','m':'c','ch':{'0':{'t':'neg','ch':{'0':{'t':'num','val':0.6886804326219642}}},'1':{'t':'exp','m':'w','ch':{'0':{'t':'x'}}}}}}},'1':{'t':'num','val':0.7433412579113017}}}"
#./makepng.py "w=140&h=140&f={'t':'sub','m':'c','ch':{'0':{'t':'cos','ch':{'0':{'t':'y'}}},'1':{'t':'add','m':'c','ch':{'0':{'t':'neg','ch':{'0':{'t':'num','val':0.6886804326219642}}},'1':{'t':'exp','m':'w','ch':{'0':{'t':'x'}}}}}}}"
#./makepng.py "w=140&h=140&f={'t':'cos','ch':{'0':{'t':'y'}}}"
#./makepng.py "w=600&h=600&f={'t':'mul','m':'w','ch':[{'t':'ccrgb','ch':[{'t':'neg','ch':[{'t':'neg','ch':[{'t':'add','m':'c','ch':[{'t':'x'},{'t':'y'}]}]}]},{'t':'log','m':'c','ch':[{'t':'ru','ch':[{'t':'div','m':'c','ch':[{'t':'sub','m':'w','ch':[{'t':'y'},{'t':'abs','ch':[{'t':'ru','ch':[{'t':'num','val':0.6102231467280965}]}]}]},{'t':'num','val':0.8530074115906249}]}]}]},{'t':'log','m':'w','ch':[{'t':'mul','m':'c','ch':[{'t':'ccrgb','ch':[{'t':'num','val':0.2323795386827796},{'t':'atan','m':'w','ch':[{'t':'y'}]},{'t':'mul','m':'c','ch':[{'t':'x'},{'t':'abs','ch':[{'t':'abs','ch':[{'t':'sin','ch':[{'t':'log','m':'c','ch':[{'t':'x'}]}]}]}]}]}]},{'t':'y'}]}]}]},{'t':'abs','ch':[{'t':'sub','m':'w','ch':[{'t':'y'},{'t':'cos','ch':[{'t':'sub','m':'w','ch':[{'t':'y'},{'t':'num','val':0.702220732211265}]}]}]}]}]}"
#./makepng.py "w=140&h=140&f={'t':'abs','ch':[{'t':'neg','ch':[{'t':'sub','m':'w','ch':[{'t':'ccrgb','ch':[{'t':'y'},{'t':'y'},{'t':'ru','ch':[{'t':'rd','ch':[{'t':'num','val':0.14073383250731575}]}]}]},{'t':'exp','m':'w','ch':[{'t':'x'}]}]}]}]}"
./makepng.py "w=140&h=140&f={'t':'cchsl','ch':[{'t':'num','val':1.0}, {'t':'num','val':0.0},{'t':'num','val':0.0}]}"
# (abs (sub-wrap y (rd (atan-wrap (sub-wrap (abs (sin x)) x)))))
