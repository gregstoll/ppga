#!/usr/bin/python3

import pngcanvas
import cgi
import jsonsinglequote
import math
import cgitb; cgitb.enable()

cache = 'postcache'

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
   'ccrgb': {'children': 3},
   'cchsl': {'children': 3}}



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

def finalHslToRgb(tc, q, p):
    if (tc < (1.0/6.0)):
        return p + ((q - p) * 6.0 * tc)
    elif (tc < (1.0/2.0)):
        return q
    elif (tc < (2.0/3.0)):
        return p + ((q - p) * ((2.0/3.0) - tc) * 6.0)
    else:
        return p

def hslToRgb(c, x, y):
    # get grayscale values for c[0], 1, 2?
    hOrig = 0.3 * c[0][0] + 0.59 * c[0][1] + 0.11 * c[0][2]
    sOrig = 0.3 * c[1][0] + 0.59 * c[1][1] + 0.11 * c[1][2]
    lOrig = 0.3 * c[2][0] + 0.59 * c[2][1] + 0.11 * c[2][2]
    # Scale h, s and l to be 0 to 1
    h = (hOrig + 1.0) / 2.0
    s = (sOrig + 1.0) / 2.0
    l = (lOrig + 1.0) / 2.0
    if (s == 0.0):
        return [(l * 2.0 - 1.0) for i in range(0, 3)]
    if (l < 0.5):
        q = l * (1.0 + s)
    else:
        q = l + s - (l * s)
    p = 2.0 * l - q
    tr = h + (1.0/3.0)
    tg = h
    tb = h - (1.0/3.0)
    if (tr < 0.0):
        tr += 1.0
    elif (tr > 1.0):
        tr -= 1.0
    if (tg < 0.0):
        tg += 1.0
    elif (tg > 1.0):
        tg -= 1.0
    if (tb < 0.0):
        tb += 1.0
    elif (tb > 1.0):
        tb -= 1.0
    cr = finalHslToRgb(tr, q, p)
    cg = finalHslToRgb(tg, q, p)
    cb = finalHslToRgb(tb, q, p)
    return [(cr * 2.0 - 1.0), (cg * 2.0 - 1.0), (cb * 2.0 - 1.0)]
    
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
        'ccrgb': lambda c, x, y: [c[0][0], c[1][1], c[2][2]],
        'cchsl': hslToRgb
    }[jsonObj['t']]

def makeTopEvaluator(jsonObj):
    eval = makeEvaluator(jsonObj)
    oldEval = eval['eval']
    eval['eval'] = lambda xs, ys: coerceResults(oldEval(xs, ys), xs, ys, eval['dep'], {'x': True, 'y': True})
    return eval

def makeEvaluator(jsonObj):
    toReturn = {}
    typeInfo = fnInfo[jsonObj['t']]
    if (typeInfo['children'] > 0):
        toReturn['c'] = [makeEvaluator(jsonObj['ch'][i]) for i in range(0, len(jsonObj['ch']))]
    # Figure out if we depend on x, y, or neither
    toReturn['dep'] = {}
    toReturn['dep']['x'] = (jsonObj['t'] == 'x')
    toReturn['dep']['y'] = (jsonObj['t'] == 'y')
    if ('c' in toReturn):
        for child in toReturn['c']:
            for xy in ['x', 'y']:
                if (child['dep'][xy]):
                    toReturn['dep'][xy] = True
    simEval = simpleEval(jsonObj)
    if ('needsMap' in typeInfo and typeInfo['needsMap']):
        mapStrat = makeMappingStrategyList(jsonObj)
    else:
        mapStrat = lambda l: l
    toReturn['eval'] = lambda xs, ys: xyAdapter(toReturn, mapStrat, simEval, xs, ys)
    return toReturn

# Results are listed in row order - (x0, y0), (x1, y0), ...,  (xn, y0), (x0, y1)
# etc.
def coerceResults(results, xs, ys, sourceDeps, targetDeps):
    coerceY = sourceDeps['y'] == False and targetDeps['y'] == True
    coerceX = sourceDeps['x'] == False and targetDeps['x'] == True
    if (coerceY):
        results = [r for y in ys for r in results]
    if (coerceX):
        results = [r for r in results for x in xs]
    return results

def xyAdapter(fnObj, mapStrat, simEval, xs, ys):
    childResults = []
    if ('c' in fnObj):
        for childObj in fnObj['c']:
            childResults.append(coerceResults(childObj['eval'](xs, ys), xs, ys, childObj['dep'], fnObj['dep']))
    if (not fnObj['dep']['x']):
        xs = [0.0]
    if (not fnObj['dep']['y']):
        ys = [0.0]
    childArgs = list(zip(*childResults))
    xys = [(x, y) for y in ys for x in xs]
    if ('c' in fnObj):
        return [mapStrat(simEval(c, x, y)) for ((x, y), c) in zip(xys, childArgs)]
    else:
        return [mapStrat(simEval([], x, y)) for (x, y) in xys]
#def makeEvaluatorOld(jsonObj):
#    return {
#       'num': (lambda jo: {'eval': (lambda this, x, y: [jo['val'] for i in range(0,3)])}),
#       'x': (lambda jo: {'eval': (lambda this, x, y: [x for i in range(0,3)])}),
#       'y': (lambda jo: {'eval': (lambda this, x, y: [y for i in range(0,3)])}),
#       'atan': (lambda jo: {'c': [makeEvaluatorOld(jsonObj['ch'][0])], 'eval': (lambda this, x, y: [makeMappingStrategy(jo)(math.atan(v)) for v in this['c'][0]['eval'](this['c'][0], x,y)])}),
#       'add': (lambda jo: {'c': [makeEvaluatorOld(jsonObj['ch'][0]), makeEvaluatorOld(jsonObj['ch'][1])], 'eval': (lambda this, x, y: [makeMappingStrategy(jo)(v1 + v2) for (v1,v2) in zip(this['c'][0]['eval'](this['c'][0],x,y), this['c'][1]['eval'](this['c'][1],x,y))])}),
#    }[jsonObj['t']](jsonObj)
            
form = cgi.FieldStorage()
func = jsonsinglequote.read(form.getfirst('f'))
width = int(form.getfirst('w'))
height = int(form.getfirst('h'))
#if (form.getfirst('saveFunc') == 't'):
     
# Don't do anything unreasonably large
if (width <= 0 or height <= 0 or width > 10000 or height > 10000):
    pass
else:
    c = pngcanvas.PNGCanvas(width, height)
    c.apply(makeTopEvaluator(func))

    print("Content-type: image/png\n")
    print(c.dump())
