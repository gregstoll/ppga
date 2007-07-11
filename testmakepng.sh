#!/bin/sh

#./makepng.py 'width=160&height=120&func={"t":"num","val":0.3}'
#./makepng.py 'width=160&height=120&func={"t":"atan","m":"c","ch":[{"t":"x"}]}'
./makepng.py "width=160&height=120&func={'t':'add','m':'c','ch':[{'t':'x'},{'t':'y'}]}"
