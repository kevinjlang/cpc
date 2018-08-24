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
    U32 rand = twoHashes[0] & 0xffdf;
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
// Streaming

Long streamingNumTrials = 10;

void streamingDoAStreamLength (Short lgK, Long n) {
  Long k = (1ULL << lgK);
  Long trialNo = 0;
  printf ("%d %lld", lgK, n); fflush (stdout);
  enum flavorType flavor;
  Long c = 0;
  Short offset;
  double avgC = 0.0;
  double avgIconEst = 0.0;
  double avgHIPEst = 0.0;
  U64 twoHashes[2]; // allocated on the stack

  for (trialNo = 0; trialNo < streamingNumTrials; trialNo++) {
    FM85 * sketch = fm85Make (lgK);
    SIMPLE85 * simple = simple85Make (lgK);
    Long i;
    for (i = 0; i < n; i++) {
      getTwoRandomHashes (twoHashes);
      fm85Update     (sketch, twoHashes[0], twoHashes[1]);
      simple85Update (simple, twoHashes[0], twoHashes[1]);
    }

    flavor = determineSketchFlavor (sketch);
    offset = sketch->windowOffset;
    avgC   += (double) sketch->numCoupons;
    avgIconEst += getIconEstimate (lgK, sketch->numCoupons);
    avgHIPEst  += sketch->hipEstAccum;

    c = sketch->numCoupons;
    assert (sketch->numCoupons == simple->numCoupons);

    U64 * matrix = bitMatrixOfSketch (sketch);
    compareU64Arrays (matrix, simple->bitMatrix, k);
    free (matrix);

    fm85Free (sketch);
    simple85Free (simple);
  }
  printf (" (%lld %d %d %.3f %.3f %.3f) okay\n", 
	  c, (int) flavor, (int) offset, // these are from the final trial
	  avgC / ((double) streamingNumTrials),
	  avgIconEst / ((double) streamingNumTrials),
	  avgHIPEst / ((double) streamingNumTrials) );
  fflush (stdout);
}

/***************************************************************/

void streamingMain (int argc, char ** argv)
{
  Short lgK;
  Long num_items;
  lgK = atoi(argv[1]);
  Long k = (1ULL << lgK);
  num_items = 0;
  while (num_items < 1200 * k) {
    streamingDoAStreamLength (lgK, num_items);
    Long prev = num_items;
    num_items = 17 * num_items / 16;
    if (num_items == prev) num_items += 1;
  }
}

/***************************************************************/
/***************************************************************/

void compressionDoAStreamLength (Short lgK, Long n) {
  Long k = (1ULL << lgK);
  Long minKN = (k < n ? k : n);
  U64 twoHashes[2]; // allocated on the stack
#ifdef TIMING
  Long numSketches = 20000000 / minKN; // was 5 million
#else
  Long numSketches = 10;
#endif
  if (numSketches < 1) numSketches = 1;
  FM85 ** streamSketches       = (FM85 **) malloc (((size_t) numSketches) * sizeof(FM85 *));
  FM85 ** compressedSketches   = (FM85 **) malloc (((size_t) numSketches) * sizeof(FM85 *));
  FM85 ** unCompressedSketches = (FM85 **) malloc (((size_t) numSketches) * sizeof(FM85 *));
  assert (streamSketches != NULL);
  assert (compressedSketches != NULL);
  assert (unCompressedSketches != NULL);
  Long sketchIndex, i;

  clock_t before1, after1, before2, after2, before3, after3;

  before1 = clock ();
  for (sketchIndex = 0; sketchIndex < numSketches; sketchIndex++) {
    FM85 * sketch = fm85Make (lgK);
    streamSketches[sketchIndex] = sketch;
    for (i = 0; i < n; i++) { 
      getTwoRandomHashes (twoHashes);
      fm85Update (sketch, twoHashes[0], twoHashes[1]);
    }
  }
  after1 = clock ();

  before2 = clock ();
  for (sketchIndex = 0; sketchIndex < numSketches; sketchIndex++) {
    compressedSketches[sketchIndex] = fm85Compress (streamSketches[sketchIndex]);
  }
  after2 = clock ();

  before3 = clock ();
  for (sketchIndex = 0; sketchIndex < numSketches; sketchIndex++) {
    unCompressedSketches[sketchIndex] = fm85Uncompress (compressedSketches[sketchIndex]);
  }
  after3 = clock ();

  double totalC = 0.0;
  double totalW = 0.0;
  for (sketchIndex = 0; sketchIndex < numSketches; sketchIndex++) {
    totalC += (double) streamSketches[sketchIndex]->numCoupons;
    totalW += (double) (compressedSketches[sketchIndex]->cwLength + compressedSketches[sketchIndex]->csvLength);
#ifndef TIMING
    assertSketchesEqual (streamSketches[sketchIndex], unCompressedSketches[sketchIndex], (Boolean) 0);
#endif
  }

  for (sketchIndex = 0; sketchIndex < numSketches; sketchIndex++) {
    fm85Free (streamSketches[sketchIndex]); streamSketches[sketchIndex] = NULL;
    fm85Free (compressedSketches[sketchIndex]); compressedSketches[sketchIndex] = NULL;
    fm85Free (unCompressedSketches[sketchIndex]); unCompressedSketches[sketchIndex] = NULL;
  }
  free (streamSketches);
  free (compressedSketches);
  free (unCompressedSketches);

  double avgC     = totalC / ((double) numSketches);
  double avgBytes = 4.0 * totalW / ((double) numSketches);

  fprintf (stdout, "(K_N_minKN_avgCoK_avgBytes %lld %lld %lld %.9f %.3f)",
	   k, n, minKN, avgC / ((double) k), avgBytes);
#ifdef TIMING
  fprintf (stdout, " %.6f %.6f %.6f\n", 
	   1e3 * ((double) (after1 - before1)) / ((double) (n * numSketches)),
	   1e3 * ((double) (after2 - before2)) / ((double) (minKN * numSketches)),
	   1e3 * ((double) (after3 - before3)) / ((double) (minKN * numSketches)) );
#else
  fprintf (stdout, "\n");
#endif
  fflush (stdout);

}

/***************************************************************/
/***************************************************************/

void compressionMain (int argc, char ** argv) {
  Short lgK;
  Long num_items;

  lgK = atoi(argv[1]);

  Long k = (1ULL << lgK);

  num_items = 0;
  while (num_items < 120 * k) {
    compressionDoAStreamLength (lgK, num_items);
    Long prev = num_items;
    num_items = 17 * num_items / 16;
    if (num_items == prev) num_items += 1;
  }

}

/***************************************************************/
/***************************************************************/
// Merging

void testMerging (Short lgKm, Short lgKa, Short lgKb, Long nA, Long nB) {
  UG85 * ugM = ug85Make (lgKm);
  U64 twoHashes[2]; // allocated on the stack

  Short lgKd = lgKm;
  if (lgKa < lgKd && nA != 0) lgKd = lgKa;
  if (lgKb < lgKd && nB != 0) lgKd = lgKb;

  FM85 * skD = fm85Make (lgKd); // direct sketch

  FM85 * skA = fm85Make (lgKa);
  FM85 * skB = fm85Make (lgKb);

  Long i = 0;

  for (i = 0; i < nA; i++) {
    getTwoRandomHashes (twoHashes);
    fm85DualUpdate (skA, skD, twoHashes[0], twoHashes[1]);
  }
  for (i = 0; i < nB; i++) {
    getTwoRandomHashes (twoHashes);
    fm85DualUpdate (skB, skD, twoHashes[0], twoHashes[1]);
  }

  ug85MergeInto (ugM, skA);
  ug85MergeInto (ugM, skB);

  Short finalLgKm = ugM->lgK;
  Boolean needToFreeMatrixM = 0;
  U64 * matrixM = bitMatrixOfUG85 (ugM, &needToFreeMatrixM);

  Long cM = countBitsSetInMatrix (matrixM, 1LL << finalLgKm);
  Long cD = skD->numCoupons;

  printf ("(lgK(MFD)AB (%d %d %d) %d %d)", lgKm, ugM->lgK, lgKd, lgKa, lgKb);
  printf ("\t(N %lld = %lld + %lld)", nA + nB, nA, nB);
  fflush (stdout);

  printf ("\t(flavorDAB %d %d %d)",
	  (int) determineSketchFlavor (skD), (int) determineSketchFlavor (skA), (int) determineSketchFlavor (skB));
  printf ("\t(cMD %lld vs %lld)\n", cM, cD);
  fflush (stdout);

  assert (finalLgKm <= lgKm);
  assert (cM <= skA->numCoupons + skB->numCoupons);
  assert (cM == cD);

  assert (finalLgKm == lgKd);
  U64 * matrixD = bitMatrixOfSketch (skD);
  Long kD = 1LL << lgKd;
  compareU64Arrays (matrixM, matrixD, kD);
  free (matrixD);

  if (needToFreeMatrixM) { free (matrixM); }

  FM85 * skR = ug85GetResult (ugM);
  assertSketchesEqual (skD, skR, (Boolean) 1);
  fm85Free (skR);

  fm85Free (skA);
  fm85Free (skB);  
  fm85Free (skD);
  ug85Free (ugM);


  return;
}

/***************************************************************/

void multiTestMerging (Short lgKm, Short lgKa, Short lgKb) {
  printf ("\nTesting lgKm = %d, lgKa = %d, lgKb = %d\n", lgKm, lgKa, lgKb);
  Long kA = 1LL << lgKa;
  Long kB = 1LL << lgKb;
  Long limA = kA * 30LL;
  Long limB = kB * 30LL;
  Long nA = 0LL;
  while (nA < limA) {
    Long nB = 0LL;
    while (nB < limB) {
      testMerging (lgKm, lgKa, lgKb, nA, nB);
      //      Long nxtB = (11 * nB) / 10;
      Long nxtB = (4 * nB) / 3;
      nB = (nxtB > (nB+1)) ? nxtB : (nB+1);
    }
    //    Long nxtA = (11 * nA) / 10;
    Long nxtA = (4 * nA) / 3;
    nA = (nxtA > (nA+1)) ? nxtA : (nA+1);
  }
  fflush (stdout);
}

/***************************************************************/

void mergingMain (int argc, char ** argv)
{
  Short lgK;

  lgK = atoi(argv[1]);

  multiTestMerging (lgK, lgK-1, lgK-1);
  multiTestMerging (lgK, lgK-1, lgK+0);
  multiTestMerging (lgK, lgK-1, lgK+1);

  multiTestMerging (lgK, lgK+0, lgK-1);
  multiTestMerging (lgK, lgK+0, lgK+0);
  multiTestMerging (lgK, lgK+0, lgK+1);

  multiTestMerging (lgK, lgK+1, lgK-1);
  multiTestMerging (lgK, lgK+1, lgK+0);
  multiTestMerging (lgK, lgK+1, lgK+1);

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
