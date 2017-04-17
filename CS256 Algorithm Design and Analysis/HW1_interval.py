#!/usr/bin/env python

# Xiang Jerry He
# HW 1
# there may be some carriage returns in this file since I use Windows
"""

Finds the largest sum subinterval of integers spread across a sequence
of files.  This also includes noting the interval itself.  The
intervals are 0-indexed and continue across files.  In other words, if
there are 10 integers in file 1 they are indexed as 0-9; the index of
the first integer in file 2 is 10.

Python documention is available at <http://docs.python.org/lib/lib.html>

Send questions to <heeringa@cs.williams.edu>

To Test on Large Data:

$ ./interval ~heeringa/scratch/256/nums1 ~heeringa/scratch/256/nums2
The largest subinterval has sum 785073483501.  It occurs at (421300,1796504).


Your program should be able to produce its results very quickly and use little space.

"""

import sys
# izip and count are helpful when iterating through a file.
from itertools import izip, count  


def LargestSumSubinterval(filenames):

    best = 0   # stores the sum of the largest interval so far
    besti = 0  # stores the left index of the best interval so far
    bestj = 0  # stores the right index of the best interval so far
    cursum = 0 # stores the sum of the current interval
    curi = 0   # stores the left index of the current interval
    offset = 0 # stores the index offset
    
    for filename in filenames:
        try:
            integers = open(filename)
        except Exception, ioe:
            sys.exit("Error opening file %s" % filename)
        else:
            for index,x in izip(count(), integers):
                x = int(x)
                if (cursum+x) > 0:
                    cursum += x
                else:
                    cursum = 0
                    curi = index+1
                if cursum > best:
                    besti = curi
                    best = cursum
                    bestj = index
    return (best, besti, bestj)
            

def usage():
    return "usage: interval filename1 [filename2 ... filenameN]\n"
    
if __name__ == '__main__':
    if len(sys.argv) > 1:
        print "The largest subinterval has sum %s.  It occurs at (%s,%s)." % LargestSumSubinterval(sys.argv[1:])
    else:
        print "The largest subinterval has sum %s.  It occurs at (%s,%s)." % LargestSumSubinterval(["nums1", "nums2"]) 
            
        
    

    
