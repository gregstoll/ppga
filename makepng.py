#!/usr/bin/python

import pngcanvas
import cgi
import json
import cgitb; cgitb.enable()

def fn(x, y):
    return [y for i in range(0, 3)]


def makeEvaluator(jsonObj):
    return {
       'num': (lambda jo: (lambda x, y: [jo['val'] for i in range(0,3)])),
       'x': (lambda jo: (lambda x, y: [x for i in range(0,3)])),
       'y': (lambda jo: (lambda x, y: [y for i in range(0,3)])),
    }[jsonObj['t']](jsonObj)
            
#if (jsonObj.type == 'number'):
#        return lambda(x, y): [(x + 0.0) for i in range(0,3)]

#print fn(0, 0, 160, 120)
#print fn(159, 0, 160, 120)
form = cgi.FieldStorage()
testJson = {'t':'num', 'val':.8}
#print json.write(testJson)
func = json.read(form.getfirst('func'))
#fn = makeEvaluator(func)
#print func
width = int(form.getfirst('width'))
height = int(form.getfirst('height'))
c = pngcanvas.PNGCanvas(width, height)
#print makeEvaluator(testJson)
#print (makeEvaluator(testJson))(0,0)
c.apply(makeEvaluator(func))
#c.color = [0xff, 0x0, 0, 0xff]
#c.filledRectangle(0, 0, c.width - 1, c.height - 1)

print "Content-type: image/png\n"
print c.dump()
#
