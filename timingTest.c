
/*

-DNDEBUG

gcc -O3 -Wall -pedantic -o timingTest u32Table.c fm85Util.c fm85.c iconEstimator.c fm85Compression.c fm85Merging.c fm85Testing.c timingTest.c

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

void do_a_stream_length (Short lgK, Long n) {
  Long k = (1ULL << lgK);
  Long minKN = (k < n ? k : n);
  Long numSketches = 20000000 / minKN; // was 20 million
  if (numSketches < 1) numSketches = 1;
  FM85 ** streamSketches       = (FM85 **) malloc (((size_t) numSketches) * sizeof(FM85 *));
  FM85 ** compressedSketches   = (FM85 **) malloc (((size_t) numSketches) * sizeof(FM85 *));
  FM85 ** unCompressedSketches = (FM85 **) malloc (((size_t) numSketches) * sizeof(FM85 *));
  assert (streamSketches != NULL);
  assert (compressedSketches != NULL);
  assert (unCompressedSketches != NULL);
  Long sketchIndex, i;
  U64 twoHashes[2]; // allocated on the stack

  clock_t before0, after0, before1, after1, before2, after2, before3, after3;

  before0 = clock ();
  for (sketchIndex = 0; sketchIndex < numSketches; sketchIndex++) {
    streamSketches[sketchIndex] = fm85Make (lgK);
  }
  after0 = clock ();

  before1 = clock ();
  for (sketchIndex = 0; sketchIndex < numSketches; sketchIndex++) {
    FM85 * sketch = streamSketches[sketchIndex];
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

  fprintf (stdout, " %.6f %.6f %.6f %.6f\n", 
	   1e3 * ((double) (after0 - before0)) / ((double) (n * numSketches)),
	   1e3 * ((double) (after1 - before1)) / ((double) (n * numSketches)),
	   1e3 * ((double) (after2 - before2)) / ((double) (minKN * numSketches)),
	   1e3 * ((double) (after3 - before3)) / ((double) (minKN * numSketches)) );

  fflush (stdout);

}




/***************************************************************/

int main (int argc, char ** argv)
{
  Short lgK;
  Long num_items;

  if (argc != 2) {
    fprintf (stderr, "Usage: %s log_k\n", argv[0]);
    return(-1);
  }

  fm85Init ();

  lgK = atoi(argv[1]);

  Long k = (1ULL << lgK);


  num_items = 10;
  while (num_items < 1200 * k) {
    do_a_stream_length (lgK, num_items);
    Long prev = num_items;
    num_items = 17 * num_items / 16;
    if (num_items == prev) num_items += 1;
  }

}
