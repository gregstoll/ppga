#!/usr/bin/python

import pngcanvas
import cgi
import jsonsinglequote
import math
import cgitb; cgitb.enable()

def fn(x, y):
    return [y for i in range(0, 3)]

# Mapping strategies ensure the result is coerced to [-1,1]
# 'c' means clip to [-1,1]
# 'w' means wrap to [-1,1]
def makeMappingStrategy(jsonObj):
    return {
            'c': (lambda v: (v > 1 and 1 or (v < -1 and -1 or v))),
            'w': (lambda v: (v == 1 and 1 or ((v+1)%2.0)-1))
    }[jsonObj['m']]

def makeMappingStrategyList(jsonObj):
    return {
            'c': (lambda l: [(v > 1 and 1 or (v < -1 and -1 or v)) for v in l]),
            'w': (lambda l: [(v == 1 and 1 or ((v+1)%2.0)-1) for v in l])
    }[jsonObj['m']]


# TODO - atan might be horribly slow?
def makeEvaluator(jsonObj):
    return {
       'num': (lambda jo: (lambda x, y: [jo['val'] for i in range(0,3)])),
       'x': (lambda jo: (lambda x, y: [x for i in range(0,3)])),
       'y': (lambda jo: (lambda x, y: [y for i in range(0,3)])),
       #'atan': (lambda jo: (lambda x, y: [makeMappingStrategy(jo)(math.atan(makeEvaluator(jsonObj['ch'][0])(x,y))) for i in range(0,3)]))
       'atan': (lambda jo: (lambda x, y: [makeMappingStrategy(jo)(math.atan(v)) for v in makeEvaluator(jsonObj['ch'][0])(x,y)])),
       'add': (lambda jo: (lambda x, y: [makeMappingStrategy(jo)(v1 + v2) for (v1,v2) in zip(makeEvaluator(jsonObj['ch'][0])(x,y), makeEvaluator(jsonObj['ch'][1])(x,y))])),
       #'add': (lambda jo: (lambda x, y: [makeMappingStrategy(jo)(v1+v2) for (v1,v2) in makeEvaluator(jsonObj['ch'][0])(x,y)]))
    }[jsonObj['t']](jsonObj)
            
#if (jsonObj.type == 'number'):
#        return lambda(x, y): [(x + 0.0) for i in range(0,3)]

#print fn(0, 0, 160, 120)
#print fn(159, 0, 160, 120)
form = cgi.FieldStorage()
testJson = {'t':'num', 'val':.8}
#print json.write(testJson)
func = jsonsinglequote.read(form.getfirst('func'))
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
