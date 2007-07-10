#!/usr/bin/python

import pngcanvas
import cgi
import json

def fn(x, y, width, height):
    return [(x + 0.0)/width for i in range(0, 3)]


def makeEvaluator(jsonObj):
    if (jsonObj.type == 'number'):
        return lambda(x, y, width, height): [(x + 0.0)/width for i in range(0,3)]

#print fn(0, 0, 160, 120)
#print fn(159, 0, 160, 120)
form = cgi.FieldStorage()
func = json.read(form.getfirst('func'))
#fn = makeEvaluator(func)
#print func
width = int(form.getfirst('width'))
height = int(form.getfirst('height'))
c = pngcanvas.PNGCanvas(width, height)
c.apply(fn)
#c.color = [0xff, 0x0, 0, 0xff]
#c.filledRectangle(0, 0, c.width - 1, c.height - 1)

print "Content-type: image/png\n"
print c.dump()
#
