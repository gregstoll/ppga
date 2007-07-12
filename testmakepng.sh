#!/bin/sh

#./makepng.py 'width=160&height=120&func={"t":"num","val":0.3}'
#./makepng.py "width=160&height=120&func={'t':'atan','m':'c','ch':[{'t':'x'}]}"
#./makepng.py "width=160&height=120&func={'t':'add','m':'c','ch':[{'t':'x'},{'t':'atan','m':'w','ch':[{'t':'y'}]}]}"
#./makepng.py "width=160&height=120&func={'t':'add','m':'c','ch':[{'t':'x'},{'t':'atan','m':'w','ch':[{'t':'y'}]}]}"
#./makepng.py "width=140&height=140&func={'t':'atan','m':'w','ch':[{'t':'log','m':'w','ch':[{'t':'ru','ch':[{'t':'x'}]}]}]}"
./makepng.py "w=140&h=140&f={'t':'mul','m':'w','ch':{'0':{'t':'sub','m':'c','ch':{'0':{'t':'cos','ch':{'0':{'t':'y'}}},'1':{'t':'add','m':'c','ch':{'0':{'t':'neg','ch':{'0':{'t':'num','val':0.6886804326219642}}},'1':{'t':'exp','m':'w','ch':{'0':{'t':'x'}}}}}}},'1':{'t':'num','val':0.7433412579113017}}}"
#./makepng.py "w=140&h=140&f={'t':'sub','m':'c','ch':{'0':{'t':'cos','ch':{'0':{'t':'y'}}},'1':{'t':'add','m':'c','ch':{'0':{'t':'neg','ch':{'0':{'t':'num','val':0.6886804326219642}}},'1':{'t':'exp','m':'w','ch':{'0':{'t':'x'}}}}}}}"
#./makepng.py "w=140&h=140&f={'t':'cos','ch':{'0':{'t':'y'}}}"
