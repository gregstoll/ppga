#!/usr/bin/python3

import os, sys, re

fns = open ('functions', 'r')
fileNum = 0
timingRe = re.compile('^([0-9.]+)user')
times = []
dirname = 'testdir'
d = 0
while os.path.exists('%s%d' %(dirname, d)):
    d = d + 1
dirname = '%s%d' %(dirname, d)
os.mkdir(dirname)
for line in fns.readlines():
    args = line[:-1]
    #cmd = '/usr/bin/time ../makepng.py "%s" > %s/png%d.png' % (args, dirname, fileNum)
    cmd = '/usr/bin/time ../cgiTester "%s" > %s/png%d.png' % (args, dirname, fileNum)
    timeTaken = 0.0
    for timeoutput in os.popen4(cmd)[1].readlines():
        timingMatch = timingRe.match(timeoutput)
        if (timingMatch):
            timeTaken = float(timingMatch.group(1))
    times.append(timeTaken)
    fileNum = fileNum + 1
fns.close()
t = open('%s/timings' % dirname, 'w')
for time in times:
    t.write('%f\n' % time)
t.close()
print(times)
# Now compare against groundtruth png's
numFiles = fileNum
for i in range(0, numFiles):
    if (os.system('/usr/bin/cmp groundtruth/png%d.png %s/png%d.png' %(i, dirname, i))):
        print("Mismatch on function number %d!" % i)
        sys.exit(1)
print("Files compare correctly.")
# Compare timings
i = 0
oldtimings = open('groundtruth/timings', 'r')
timeTotals = [0.0, 0.0]
for oldT in oldtimings.readlines():
    oldtime = float(oldT[:-1])
    print("oldtime is %f, newtime is %f, factor is %f" % (oldtime, times[i], times[i]/oldtime))
    timeTotals[0] += oldtime
    timeTotals[1] += times[i]
    i = i + 1
print("Total timings: old %f, new %f, factor %f" % (timeTotals[0], timeTotals[1], timeTotals[1]/timeTotals[0]))
