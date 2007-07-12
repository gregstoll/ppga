#!/usr/bin/python
import string

info = []

def main(inFile):
    f = open(inFile, 'r')
    numArgs = 0
    for line in f.readlines():
        curLine = line[:-1]
        if (curLine.endswith('arg:')):
            numArgs = int(string.strip(curLine).split(' ')[0])
        elif ('-' in curLine):
            dashPos = curLine.index('-')
            origName = string.strip(curLine[:dashPos])
            rightSide = string.strip(curLine[dashPos+1:])
            rsSplit = rightSide.split(' ')
            if (rsSplit[0] != '??'):
                name = rsSplit[0]
                if (name == 'def'):
                    name = origName
                needsMap = (rsSplit[-1] == '*')
                info.append({'name': name, 'needsMap': needsMap, 'children': numArgs})
    #print info
    f.close()
    # Javascript
    for entry in info:
        print "{'type': '%s', 'children': %d%s}," % (entry['name'], entry['children'], (entry['needsMap'] and ", 'needsMap': true" or ""))
    # Python
    for entry in info:
        print "'%s': {'children': %d%s}," % (entry['name'], entry['children'], (entry['needsMap'] and ", 'needsMap' : True" or ""))

if (__name__ == '__main__'):
    main('newfuncs')
