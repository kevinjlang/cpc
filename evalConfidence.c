// Copyright 2018, Kevin Lang, Oath Research

/*

  gcc -O3 -Wall -pedantic -o evalConfidence u32Table.c fm85Util.c fm85.c iconEstimator.c fm85Confidence.c fm85Compression.c fm85Merging.c fm85Testing.c evalConfidence.c

*/

/*******************************************************/

#include "common.h"
#include "fm85Util.h"
#include "u32Table.h"
#include "fm85.h"
#include "iconEstimator.h"
#include "fm85Confidence.h"
#include "fm85Compression.h"
#include "fm85Merging.h"
#include "fm85Testing.h"

/***************************************************************/

Long * chooseTargetNs (Long maxN, Long * returnLen) {
  Long num = 0;
  Long cur = 0;
  while (cur < maxN) {
    Long nxt = 17 * cur / 16;
    cur += 1;
    if (nxt > cur) cur = nxt;
    num += 1;
  }
  Long len = num;
  *returnLen = len;
  num = 0;
  cur = 0;
  Long * arr = malloc (len * sizeof(Long));
  while (cur < maxN) {
    Long nxt = 17 * cur / 16;
    cur += 1;
    if (nxt > cur) cur = nxt;
    arr[num] = cur;
    num += 1;
  }
  return (arr);
}

/***************************************************************/

int main (int argc, char ** argv)
{
  Short lgK;
  Long numTrials;
  int kappa;
  U64 twoHashes[2];
  Long trialNo;
  Long targetI;
  Long itemI;

  if (argc != 3) {
    fprintf (stderr, "Usage: %s log_k numTrials\n", argv[0]);
    return(-1);
  }

  fm85Init ();

  lgK = atoi(argv[1]);
  numTrials = atol(argv[2]);

  Long k = 1LL << lgK;
  Long numTargetNs;
  Long * targetNs = chooseTargetNs(1000LL * k, &numTargetNs);

  Long maxN = targetNs[numTargetNs-1];

  Long *iconOutLoCount[4] = {NULL, NULL, NULL, NULL};
  Long *iconOutHiCount[4] = {NULL, NULL, NULL, NULL};
  Long  *hipOutLoCount[4] = {NULL, NULL, NULL, NULL};
  Long  *hipOutHiCount[4] = {NULL, NULL, NULL, NULL};

  for (kappa = 1; kappa <= 3; kappa++) {
    iconOutLoCount[kappa] = calloc(numTargetNs, sizeof(Long)); // zero filled
    iconOutHiCount[kappa] = calloc(numTargetNs, sizeof(Long));
    hipOutLoCount[kappa] = calloc(numTargetNs, sizeof(Long));
    hipOutHiCount[kappa] = calloc(numTargetNs, sizeof(Long));
  }

  for (trialNo = 0; trialNo < numTrials; trialNo++) {
    FM85 * sketch = fm85Make (lgK);
    
    targetI = 0;
    for (itemI = 1; itemI <= maxN; itemI++) {
      getTwoRandomHashes (twoHashes);
      fm85Update (sketch, twoHashes[0], twoHashes[1]);
      if (itemI == targetNs[targetI]) {
	double dn = (double) targetNs[targetI];
	for (kappa = 1; kappa <= 3; kappa++) {
	  if (dn < getIconConfidenceLB(sketch, kappa)) { iconOutLoCount[kappa][targetI]++; }
	  if (dn > getIconConfidenceUB(sketch, kappa)) { iconOutHiCount[kappa][targetI]++; }
	  if (dn <  getHIPConfidenceLB(sketch, kappa)) {  hipOutLoCount[kappa][targetI]++; }
	  if (dn >  getHIPConfidenceUB(sketch, kappa)) {  hipOutHiCount[kappa][targetI]++; }
	}
	targetI++;
      }
    }
    fm85Free(sketch);
  }

  for (targetI = 0; targetI < numTargetNs; targetI++) {
    printf ("%lld\t%lld", targetNs[targetI], k);
    printf ("\t%.9f\t%.9f\t%.9f\t%.9f",
	    ((double) iconOutLoCount[2][targetI]) / ((double) numTrials),
	    ((double) iconOutHiCount[2][targetI]) / ((double) numTrials),
	    ((double)  hipOutLoCount[2][targetI]) / ((double) numTrials),
	    ((double)  hipOutHiCount[2][targetI]) / ((double) numTrials));
    printf ("\t%.9f\t%.9f\t%.9f\t%.9f",
	    ((double) iconOutLoCount[1][targetI]) / ((double) numTrials),
	    ((double) iconOutHiCount[1][targetI]) / ((double) numTrials),
	    ((double)  hipOutLoCount[1][targetI]) / ((double) numTrials),
	    ((double)  hipOutHiCount[1][targetI]) / ((double) numTrials));
    printf ("\t%.9f\t%.9f\t%.9f\t%.9f",
	    ((double) iconOutLoCount[3][targetI]) / ((double) numTrials),
	    ((double) iconOutHiCount[3][targetI]) / ((double) numTrials),
	    ((double)  hipOutLoCount[3][targetI]) / ((double) numTrials),
	    ((double)  hipOutHiCount[3][targetI]) / ((double) numTrials));
    printf ("\n");
  }


}
