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


#def returnAtanEval(jsonObj):
#    assert (jsonObj['t'] == 'atan')
#    ch0Eval = makeEvaluator(jsonObj['ch'][0])
#    return 

def makeEvaluator(jsonObj):
    return {
       'num': (lambda jo: {'eval': (lambda this, x, y: [jo['val'] for i in range(0,3)])}),
       'x': (lambda jo: {'eval': (lambda this, x, y: [x for i in range(0,3)])}),
       'y': (lambda jo: {'eval': (lambda this, x, y: [y for i in range(0,3)])}),
       #'atan': (lambda jo: (lambda x, y: [makeMappingStrategy(jo)(math.atan(makeEvaluator(jsonObj['ch'][0])(x,y))) for i in range(0,3)]))
       'atan': (lambda jo: {'c': [makeEvaluator(jsonObj['ch'][0])], 'eval': (lambda this, x, y: [makeMappingStrategy(jo)(math.atan(v)) for v in this['c'][0]['eval'](this['c'][0], x,y)])}),
       'add': (lambda jo: {'c': [makeEvaluator(jsonObj['ch'][0]), makeEvaluator(jsonObj['ch'][0])], 'eval': (lambda this, x, y: [makeMappingStrategy(jo)(v1 + v2) for (v1,v2) in zip(this['c'][0]['eval'](this['c'][0],x,y), this['c'][1]['eval'](this['c'][1],x,y))])}),
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

print "Content-type: image/png\n"
print c.dump()
#
