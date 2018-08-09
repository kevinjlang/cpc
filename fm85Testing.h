// Copyright 2018, Kevin Lang, Oath Research

#ifndef GOT_FM85TESTING_H

#include "common.h"
#include "fm85.h"

/*******************************************************/
// This simple (but naive) implementation of FM85 sketches can
// provide ground truth for testing the fancy implementation.

typedef struct simple85_sketch_type
{
  Short lgK;
  U64 * bitMatrix;
  Long numCoupons;
} SIMPLE85;

SIMPLE85 * simple85Make (Short lgK);

void simple85Free (SIMPLE85 * self);

void simple85Update (SIMPLE85 * self, U64 hash0, U64 hash1);

/*******************************************************/

void getTwoRandomHashes (U64 twoHashes[]);

// supports different k values for the two sketches
void fm85DualUpdate (FM85 * sk1, FM85 * sk2, U64 hash0, U64 hash1);

/*******************************************************/

void compareByteArrays(U8 * arr1, U8 * arr2, Long arrlen);
void compareU32Arrays(U32 * arr1, U32 * arr2, Long arrlen);
void compareU64Arrays(U64 * arr1, U64 * arr2, Long arrlen);

// for testing, especially of the merging code
void assertSketchesEqual (FM85 * sk1, FM85 * sk2, Boolean sk2WasMerged);

#define GOT_FM85TESTING_H
#endif
