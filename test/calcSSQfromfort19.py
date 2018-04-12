#!/usr/bin/env python
############################################################################
#
#
#
# Calculate the SSQ from the input file from command line fort.19
#
#
############################################################################

import sys

ifpath = sys.argv[1]

ifi = open(ifpath, 'r')
ssq = 0.0

for i in range(0,3):
   ifi.readline()
while True:
   line = list(ifi.readline())
   if not line:
      print "ERROR"
      exit()
   idn = int("".join(line[0:3]))
   print(idn)
   if idn in [22,99]: break
   calc = float("".join(line[9:20]))
   obsv = float("".join(line[20:30]))
   ssq += (calc - obsv) ** 2

print(ssq)
