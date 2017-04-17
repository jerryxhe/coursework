#!/usr/bin/env python
# Xiang Jerry He

# Pretty Printer
# Dynamic programming implemented as memoized recursion

import sys
sys.setrecursionlimit(10**7)

def pp(L, filename):
    words=open(filename).read().split()
    wordlens = map(len, words)
    last = bestSlack(wordlens, L)
    start = 0; end = start
    while end < len(wordlens):
        end = last[start]+1
        print " ".join(words[start:end])
        start = end

    
def bestSlack(wordlens, L, i=None, optlsr=[], level=0):
    nword = len(wordlens)
    if i is None: i = nword
    last = [None]*i
    if level==0:
        optlsr = [None]*(nword+1)
    if i==0:
        return (0, [])
    else:
        assert(i>0)
        linelensofar = wordlens[-i]
        mintup = (optlsr[i-1] or bestSlack(wordlens, L, i-1, optlsr, level+1))
        minsofar = mintup[0] + (L-linelensofar)**2
        last[-i] = nword-i
        if i > 1: last[(-i+1):] = mintup[1]
        j = i;j-=1; linelensofar += (wordlens[-j]+1)
        while(linelensofar <= L and j > 0):
            cantup = (optlsr[j-1] or bestSlack(wordlens, L, j-1, optlsr, level+1))
            candidate = cantup[0] + (L-linelensofar)**2
            if candidate < minsofar:
                minsofar = candidate
                last[-i] = nword-j
                if cantup[1] != []:
                    last[(-j+1):] = cantup[1]
            j-=1; linelensofar += (wordlens[-j]+1)
        optlsr[i] = (minsofar, last)
        if level==0:
            return last
        else:
            return (minsofar, last)

if __name__=="__main__":
    pp(int(sys.argv[1]), sys.argv[2])
