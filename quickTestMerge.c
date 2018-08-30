// Copyright 2018, Kevin Lang, Oath Research

/*
  This test of merging is less exhaustive than testAll.c, 
  but is more practical for large values of K.

  gcc -O3 -Wall -pedantic -o quickTestMerge u32Table.c fm85Util.c fm85.c iconEstimator.c fm85Compression.c fm85Merging.c fm85Testing.c quickTestMerge.c

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

/***************************************************************/
/***************************************************************/
// Quick Test of Merging (equal K case)

void quickTest (Short lgK, Long cA, Long cB) {
  U64 twoHashes[2]; // allocated on the stack
  FM85 * skA = fm85Make (lgK);
  FM85 * skB = fm85Make (lgK);
  FM85 * skD = fm85Make (lgK); // direct sketch
  Long nA = 0;
  Long nB = 0;

  clock_t t0, t1, t2, t3, t4, t5;

  //  printf ("0"); fflush (stdout);
  t0 = clock();
  while (skA->numCoupons < cA) {
    nA++;
    getTwoRandomHashes (twoHashes);
    fm85DualUpdate (skA, skD, twoHashes[0], twoHashes[1]);
  }
  printf (" A"); fflush (stdout);
  t1 = clock();
  while (skB->numCoupons < cB) {
    nB++;
    getTwoRandomHashes (twoHashes);
    fm85DualUpdate (skB, skD, twoHashes[0], twoHashes[1]);
  }
  printf ("B"); fflush (stdout);
  t2 = clock();

  UG85 * ugM = ug85Make (lgK);
  ug85MergeInto (ugM, skA);
  printf (" A"); fflush (stdout);
  t3 = clock();

  ug85MergeInto (ugM, skB);
  printf ("B"); fflush (stdout);
  t4 = clock();

  FM85 * skR = ug85GetResult (ugM);
  printf (" R "); fflush (stdout);
  t5 = clock();

  //  printf ("(lgK %d)\t", (int) lgK);
  //  printf ("(N %lld = %lld + %lld)\t", nA + nB, nA, nB);
  //  printf ("(C %lld vs %lld (%lld %lld))\t", skR->numCoupons, skD->numCoupons, skA->numCoupons, skB->numCoupons);
  //  printf ("(flavorDAB %d %d %d)\n", (int) determineSketchFlavor (skD), (int) determineSketchFlavor (skA), (int) determineSketchFlavor (skB));
  fflush (stdout);
  assert (skR->numCoupons == skD->numCoupons);
  assertSketchesEqual (skD, skR, (Boolean) 1);

  printf (" (%.1f %.1f) (%.1f %.1f) %.1f\n",
	  //	  500.0 * ((double) (t1 - t0)) / ((double) nA), // 500 is 1000 / 2
	  //	  500.0 * ((double) (t2 - t1)) / ((double) nB),
	  ((double) (t1 - t0)) / 2000.0,
	  ((double) (t2 - t1)) / 2000.0,

	  ((double) (t3 - t2)) / 1000.0,
	  ((double) (t4 - t3)) / 1000.0,

	  ((double) (t5 - t4)) / 1000.0);

  fm85Free (skR);
  fm85Free (skA);
  fm85Free (skB);  
  fm85Free (skD);
  ug85Free (ugM);

  return;
}

/***************************************************************/
/*
  Note: In terms of normalized nanoseconds, the two stream processing
  costs are fairly reasonable, although 50 nsec seems slightly high.
 */
/***************************************************************/

// #define numCvalues 5
#define numCvalues 6

void multiQuickTest (Short lgK) {
  Long k = 1LL << lgK;
  Long targetC [numCvalues] = {0LL, 1LL, (3*k/32)-1, k/3, k, (7*k)/2};
  //  Long targetC [numCvalues] = {0LL, k/16, k/4, k, (7*k)/2};
  //  Long targetC [numCvalues] = {0LL, k/16, k/4, 2*k, 3*k, 4*k};
  int i, j;
  for (i = 0; i < numCvalues; i++) {
    printf("\n"); fflush (stdout);
    for (j = 0; j < numCvalues; j++) {
      printf("%d %d", i, j); fflush (stdout);
      quickTest (lgK, targetC[i], targetC[j]);
    }
  }
  fflush (stdout);
}

/***************************************************************/

int main (int argc, char ** argv)
{
  Short lgK;
  if (argc != 2) {
    fprintf (stderr, "Usage: %s log_k\n", argv[0]);
    return(-1);
  }

  lgK = atoi(argv[1]);
  fm85Init ();
  multiQuickTest (lgK);
}


