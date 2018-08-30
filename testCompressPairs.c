// Copyright 2018, Kevin Lang, Oath Research

/*

 gcc -DLOW_LEVEL_CRAP -O3 -Wall -pedantic -o testCompressPairs u32Table.c fm85Util.c fm85.c iconEstimator.c fm85Compression.c fm85Merging.c fm85Testing.c testCompressPairs.c

*/

/*******************************************************/

#include "common.h"
#include "fm85Util.h"
#include "u32Table.h"
#include "fm85.h"
#include "iconEstimator.h"
#include "fm85Compression.h"
#include "fm85Merging.h"
#include "fm85Testing.h"

#define K 1024
#define N  200
#define MAXWORDS 1000

void doTheTest (void) {
  U64 twoHashes[2];
  U32 pairArray[N];
  U32 pairArray2[N];
  int i;
  for (i = 0; i < N; i++) {
    getTwoRandomHashes(twoHashes);
    U32 rand = twoHashes[0] & 0xffff;   // was 0xffdf;
    pairArray[i] = rand;
  }
  u32KnuthShellSort3(pairArray, 0L, (Long) (N-1));  // unsigned numerical sort
  U32 prev = ALL32BITS;
  int nxt = 0;
  for (i = 0; i < N; i++) {     // uniquify
    if (pairArray[i] != prev) {
      prev = pairArray[i];
      pairArray[nxt++] = pairArray[i];
    }
  }
  int numPairs = nxt;
  printf ("numPairs = %d\n", numPairs);

  //  for (i = 0; i < numPairs; i++) {
  //    printf ("%d: %d %d\n", i, pairArray[i] >> 6, pairArray[i] & 63);
  //  }

  U32 compressedWords [MAXWORDS];
  Long bb; // numBaseBits

  for (bb = 0; bb <= 11; bb++) {
    Long numWordsWritten = 
      lowLevelCompressPairs (pairArray, (Long) numPairs, bb, compressedWords);
    printf ("numWordsWritten = %lld (bb = %lld)\n", numWordsWritten, bb); 
						
    lowLevelUncompressPairs (pairArray2, (Long) numPairs, bb, compressedWords,
			     numWordsWritten);
    for (i = 0; i < numPairs; i++) {			   
      assert (pairArray[i] == pairArray2[i]);
    }
  }
			   
}


/***************************************************************/
/***************************************************************/

int main (int argc, char ** argv) {
  if (argc != 1) {
    fprintf (stderr, "Usage: %s\n", argv[0]);
    return(-1);
  }
  fm85Init ();
  doTheTest ();
}
