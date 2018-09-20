// Copyright 2018, Kevin Lang, Oath Research

#include "fm85Util.h"
#include "fm85Testing.h"

/*******************************************************/

#include "mycity.h"

static const U64 golden64 = 0x9e3779b97f4a7c13ULL;  // the golden ratio
static const U64 oddnum64 = 0xe0685c665a8eb357ULL;  // an odd random number

U64 counter0 =  35538947; // some arbitrary
U64 counter1 = 796576885; // starting values

void getTwoRandomHashes (U64 twoHashes[]) {
  twoHashes[0] = MyCity64(counter0); counter0 += golden64;
  twoHashes[1] = MyCity64(counter1); counter1 += oddnum64;
}

// U32 generateRandomRowCol (Short lgK) {
//   U64 hash0 = MyCity64(counter_0); counter_0 += golden64;
//   U64 hash1 = MyCity64(counter_1); counter_1 += oddnum64;
//   return (rowColFromTwoHashes(hash0, hash1, lgK));
// }

/*******************************************************/
// This simple (but naive) implementation of FM85 sketches can
// provide ground truth for testing the fancy implementation.

SIMPLE85 * simple85Make (Short lgK) {
  assert (lgK >= 4 && lgK <= 26);
  Long k = (1LL << lgK);
  U64 * matrix = (U64 *) malloc ( ((size_t) k) * sizeof(U64) );
  assert (matrix != NULL);
  bzero ((void *) matrix, ((size_t) k) * sizeof(U64) );
  SIMPLE85 * self = (SIMPLE85 *) malloc (sizeof(SIMPLE85));
  assert (self != NULL);
  self->bitMatrix = matrix;
  self->numCoupons = 0;
  self->lgK = lgK;
  return (self);
}

void simple85Free (SIMPLE85 * self) {
  assert (self != NULL);
  assert (self->bitMatrix != NULL);
  free (self->bitMatrix);
  free (self);
}

void simple85RowColUpdate (SIMPLE85 * self, U32 rowCol) {
  Short col = (Short) (rowCol & 63);
  Long  row = (Long)  (rowCol >> 6);  
  U64 oldPattern = self->bitMatrix[row];
  U64 newPattern = oldPattern | (1LL << col);
  if (newPattern != oldPattern) { self->numCoupons++; }
  self->bitMatrix[row] = newPattern;
}

void simple85Update (SIMPLE85 * self, U64 hash0, U64 hash1) {
  U32 rowCol = rowColFromTwoHashes (hash0, hash1, self->lgK);  
  simple85RowColUpdate (self, rowCol);
}

/*******************************************************/
/*******************************************************/

void fm85DualUpdate (FM85 * sk1, FM85 * sk2, U64 hash0, U64 hash1) {
  U32 rowCol = rowColFromTwoHashes (hash0, hash1, (Short) 26); // notice the 26
  U32 mask1 = (((1 << sk1->lgK) - 1) << 6) | 63;
  U32 mask2 = (((1 << sk2->lgK) - 1) << 6) | 63;
  fm85RowColUpdate (sk1, rowCol & mask1);
  fm85RowColUpdate (sk2, rowCol & mask2);
}

/*******************************************************/
/*******************************************************/

void compareByteArrays(U8 * arr1, U8 * arr2, Long arrlen) {
  int i = 0;
  for (i = 0; i < arrlen; i++) {
    if (arr1[i] != arr2[i])
      FATAL_ERROR("The U8 arrays don't match");
  }
}

void compareU32Arrays(U32 * arr1, U32 * arr2, Long arrlen) {
  int i = 0;
  for (i = 0; i < arrlen; i++) {
    if (arr1[i] != arr2[i])
      FATAL_ERROR("The U32 arrays don't match");
  }
}

void compareU64Arrays(U64 * arr1, U64 * arr2, Long arrlen) {
  int i = 0;
  for (i = 0; i < arrlen; i++) {
    if (arr1[i] != arr2[i])
      FATAL_ERROR("The U64 arrays don't match");
  }
}

/*******************************************************/
/*******************************************************/

// This procedure is only used for testing.
// The actual sketch code calculates firstInterestingColumn
// "on the side" while accomplishing something else.

Short calculateFirstInterestingColumn (FM85 * self) {
  Short offset = self->windowOffset;
  if (offset == 0) return 0;
  u32Table * table = self->surprisingValueTable;
  assert (table != NULL);
  U32 * slots = table->slots;
  Long numSlots = (1LL << table->lgSize); 
  Long i;
  Short result = offset;
  for (i = 0; i < numSlots; i++) { 
    U32 rowCol = slots[i];
    if (rowCol != ALL32BITS) {
      Short col = (Short) (rowCol & 63);
      if (col < result) { result = col; }
    }
  }
  return(result);
}

/*******************************************************/
/*******************************************************/
// This is used for testing, especially of the merging code.

void assertSketchesEqual (FM85 * sk1, FM85 * sk2, Boolean sk2WasMerged) {
  //  if (sk1->lgK != sk2->lgK) { printf ("%d vs %d\n", (int) sk1->lgK, (int) sk2->lgK); fflush (stdout); }
  assert (sk1->lgK == sk2->lgK);
  Long k = 1LL << sk1->lgK;
  assert (sk1->isCompressed == sk2->isCompressed);
  assert (sk1->numCoupons == sk2->numCoupons);
  assert (sk1->windowOffset == sk2->windowOffset);
  assert (sk1->cwLength == sk2->cwLength);
  assert (sk1->csvLength == sk2->csvLength);
  assert (sk1->numCompressedSurprisingValues == sk2->numCompressedSurprisingValues);

  if (sk1->surprisingValueTable != NULL || sk2->surprisingValueTable != NULL) {
    assert (sk1->surprisingValueTable != NULL && sk2->surprisingValueTable != NULL);
    Long numPairs1 = 0; 
    Long numPairs2 = 0; 
    U32 * pairs1 = u32TableUnwrappingGetItems (sk1->surprisingValueTable, &numPairs1);
    U32 * pairs2 = u32TableUnwrappingGetItems (sk2->surprisingValueTable, &numPairs2);
    introspectiveInsertionSort(pairs1, 0, numPairs1 - 1);
    introspectiveInsertionSort(pairs2, 0, numPairs2 - 1);
    assert (numPairs1 == numPairs2);
    compareU32Arrays (pairs1, pairs2, numPairs1);
    free (pairs1);
    free (pairs2);
  }

  if (sk1->slidingWindow != NULL || sk2->slidingWindow != NULL) {
    assert (sk1->slidingWindow != NULL && sk2->slidingWindow != NULL);
    compareByteArrays (sk1->slidingWindow, sk2->slidingWindow, k);
  }

  if (sk1->compressedWindow != NULL || sk2->compressedWindow != NULL) {
    assert (sk1->compressedWindow != NULL && sk2->compressedWindow != NULL);
    compareU32Arrays (sk1->compressedWindow, sk2->compressedWindow, k);
  }

  if (sk1->compressedSurprisingValues != NULL || sk2->compressedSurprisingValues != NULL) {
    assert (sk1->compressedSurprisingValues != NULL && sk2->compressedSurprisingValues != NULL);
    compareU32Arrays (sk1->compressedSurprisingValues, sk2->compressedSurprisingValues, k);
  }

  if (sk2WasMerged) {
    assert (sk1->mergeFlag == 0 && sk2->mergeFlag == 1);
    // firstInterestingColumn is only updated occasionally while stream processing.
    // Therefore the merged sketch's value might be more up-to-date than that of the direct sketch.
    assert (sk2->firstInterestingColumn == calculateFirstInterestingColumn (sk1));
  }
  else {
    assert (sk1->mergeFlag == sk2->mergeFlag);
    assert (sk1->firstInterestingColumn == sk2->firstInterestingColumn);
    assert (sk1->kxp == sk2->kxp);                 // TODO: deal with the 
    assert (sk1->hipEstAccum == sk2->hipEstAccum); // floating point issues
    assert (sk1->hipErrAccum == sk2->hipErrAccum); // involving these three.
  }

}
