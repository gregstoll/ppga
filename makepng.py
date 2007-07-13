#!/usr/bin/python

import pngcanvas
import cgi
import jsonsinglequote
import math
import cgitb; cgitb.enable()

def if_else(condition, trueVal, falseVal):
    if condition:
        return trueVal
    else:
        return falseVal

def safe_ternary(x, conditionfn, truefn, falsefn):
    if (conditionfn(x)):
        return truefn(x)
    else:
        return falsefn(x)

fnInfo = {
   'num': {'children': 0, 'aux': 'val'},
   'x': {'children': 0},
   'y': {'children': 0},
   'atan': {'children': 1, 'needsMap' : True},
   'abs': {'children': 1},
   'cos': {'children': 1},
   'exp': {'children': 1, 'needsMap' : True},
   'log': {'children': 1, 'needsMap' : True},
   'neg': {'children': 1},
   'rd': {'children': 1},
   'ru': {'children': 1},
   'sin': {'children': 1},
   'add': {'children': 2, 'needsMap' : True},
   'div': {'children': 2, 'needsMap' : True},
   'mul': {'children': 2, 'needsMap' : True},
   'sub': {'children': 2, 'needsMap' : True},
   'cc': {'children': 3}}



# Mapping strategies ensure the result is coerced to [-1,1]
# 'c' means clip to [-1,1]
# 'w' means wrap to [-1,1]
def makeMappingStrategy(jsonObj):
    return {
        'c': (lambda v: (if_else(v > 1, 1, if_else(v < -1, -1, v)))),
        'w': (lambda v: (if_else(v == 1, 1, ((v+1)%2.0)-1)))
    }[jsonObj['m']]

def makeMappingStrategyList(jsonObj):
    return {
        'c': (lambda l: [if_else(v > 1, 1, if_else(v < -1, -1, v)) for v in l]),
        'w': (lambda l: [if_else(v == 1, 1, ((v+1)%2.0)-1) for v in l])
    }[jsonObj['m']]



def simpleEval(jsonObj):
    return {
        'num': lambda c, x, y: [jsonObj['val'] for i in range(0,3)],
        'x' : lambda c, x, y: [x for i in range(0,3)],
        'y': lambda c, x, y: [y for i in range(0,3)],
        'atan': lambda c, x, y: [math.atan(c[0][i]) for i in range(0,3)],
        'abs': lambda c, x, y: [abs(c[0][i]) for i in range(0,3)],
        'cos': lambda c, x, y: [math.cos(c[0][i]) for i in range(0, 3)],
        'exp': lambda c, x, y: [math.exp(c[0][i]) for i in range(0, 3)],
        'log': lambda c, x, y: [safe_ternary(c[0][1], lambda x: x <= 0.0, lambda x: 0, lambda x: math.log(x)) for i in range(0, 3)],
        'neg': lambda c, x, y: [-1.0 * c[0][i] for i in range(0, 3)],
        'rd': lambda c, x, y: [math.floor(c[0][i]) for i in range(0, 3)],
        'ru': lambda c, x, y: [math.ceil(c[0][i]) for i in range(0, 3)],
        'sin': lambda c, x, y: [math.sin(c[0][i]) for i in range(0, 3)],
        'add': lambda c, x, y: [c[0][i] + c[1][i] for i in range(0, 3)],
        'div': lambda c, x, y: [safe_ternary((c[0][1], c[1][i]), lambda x : x[1] == 0.0, lambda x: 0, lambda x: x[0]/x[1]) for i in range(0, 3)],
        'mul': lambda c, x, y: [c[0][i] * c[1][i] for i in range(0, 3)],
        'sub': lambda c, x, y: [c[0][i] - c[1][i] for i in range(0, 3)],
        'cc': lambda c, x, y: [c[0][0], c[1][1], c[2][2]]
    }[jsonObj['t']]

def makeEvaluator(jsonObj):
    toReturn = {}
    typeInfo = fnInfo[jsonObj['t']]
    if (typeInfo['children'] > 0):
        toReturn['c'] = [makeEvaluator(jsonObj['ch'][i]) for i in range(0, len(jsonObj['ch']))]
    simEval = simpleEval(jsonObj)
    if ('needsMap' in typeInfo and typeInfo['needsMap']):
        mapStrat = makeMappingStrategyList(jsonObj)
    else:
        mapStrat = lambda l: l
    if (typeInfo['children'] > 0):
        toReturn['eval'] = lambda x, y: mapStrat(simEval([toReturn['c'][i]['eval'](x,y) for i in range(0, len(toReturn['c']))], x, y))
    else:
        toReturn['eval'] = lambda x, y: mapStrat(simEval([], x, y))
    return toReturn

def makeEvaluatorOld(jsonObj):
    return {
       'num': (lambda jo: {'eval': (lambda this, x, y: [jo['val'] for i in range(0,3)])}),
       'x': (lambda jo: {'eval': (lambda this, x, y: [x for i in range(0,3)])}),
       'y': (lambda jo: {'eval': (lambda this, x, y: [y for i in range(0,3)])}),
       'atan': (lambda jo: {'c': [makeEvaluatorOld(jsonObj['ch'][0])], 'eval': (lambda this, x, y: [makeMappingStrategy(jo)(math.atan(v)) for v in this['c'][0]['eval'](this['c'][0], x,y)])}),
       'add': (lambda jo: {'c': [makeEvaluatorOld(jsonObj['ch'][0]), makeEvaluatorOld(jsonObj['ch'][1])], 'eval': (lambda this, x, y: [makeMappingStrategy(jo)(v1 + v2) for (v1,v2) in zip(this['c'][0]['eval'](this['c'][0],x,y), this['c'][1]['eval'](this['c'][1],x,y))])}),
    }[jsonObj['t']](jsonObj)
            
form = cgi.FieldStorage()
testJson = {'t':'num', 'val':.8}
#print json.write(testJson)
func = jsonsinglequote.read(form.getfirst('f'))
#fn = makeEvaluator(func)
#print func
width = int(form.getfirst('w'))
height = int(form.getfirst('h'))
# Don't do anything unreasonably large
if (width <= 0 or height <= 0 or width > 10000 or height > 10000):
    pass
else:
    c = pngcanvas.PNGCanvas(width, height)
    #print makeEvaluator(testJson)
    #print (makeEvaluator(testJson))(0,0)
    c.apply(makeEvaluator(func))

    print "Content-type: image/png\n"
    print c.dump()
