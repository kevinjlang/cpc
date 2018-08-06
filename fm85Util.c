// Copyright 2018, Kevin Lang, Oath Research

#include "fm85Util.h"

/******************************************/

void * shallowCopy (void * oldObject, size_t numBytes) {
  if (oldObject == NULL || numBytes == 0) { FATAL_ERROR ("shallowCopyObject: bad arguments"); }
  void * newObject = malloc (numBytes);
  if (newObject == NULL) { FATAL_ERROR ("shallowCopyObject: malloc failed"); }
  memcpy (newObject, oldObject, numBytes);
  return (newObject);
}

/******************************************/

U8 byteLeadingZerosTable[256];

U8 slow_count_leading_zeros_in_byte (U8 the_byte)
{
  int shift;
  int count = 0;
  for (shift = 7; shift >= 0; shift--) {
    int is_zero = !((the_byte >> shift) & 1);
    if (is_zero)
      count++;
    else
      break;
  }
  return count;
}

void fillByteLeadingZerosTable(void)
{
  int j;
  for (j = 0; j < 256; j++)
    byteLeadingZerosTable[j] = slow_count_leading_zeros_in_byte ((U8) j);
}

/******************************************/

#define FCLZ_MASK_56 ((U64) 0x00ffffffffffffff)
#define FCLZ_MASK_48 ((U64) 0x0000ffffffffffff)
#define FCLZ_MASK_40 ((U64) 0x000000ffffffffff)
#define FCLZ_MASK_32 ((U64) 0x00000000ffffffff)
#define FCLZ_MASK_24 ((U64) 0x0000000000ffffff)
#define FCLZ_MASK_16 ((U64) 0x000000000000ffff)
#define FCLZ_MASK_08 ((U64) 0x00000000000000ff)

Short countLeadingZerosInUnsignedLong (U64 theInput) {
  if (theInput > FCLZ_MASK_56)
    return ((Short) ( 0 + byteLeadingZerosTable[(theInput >> 56) & FCLZ_MASK_08]));
  if (theInput > FCLZ_MASK_48)
    return ((Short) ( 8 + byteLeadingZerosTable[(theInput >> 48) & FCLZ_MASK_08]));
  if (theInput > FCLZ_MASK_40)
    return ((Short) (16 + byteLeadingZerosTable[(theInput >> 40) & FCLZ_MASK_08]));
  if (theInput > FCLZ_MASK_32)
    return ((Short) (24 + byteLeadingZerosTable[(theInput >> 32) & FCLZ_MASK_08]));
  if (theInput > FCLZ_MASK_24)
    return ((Short) (32 + byteLeadingZerosTable[(theInput >> 24) & FCLZ_MASK_08]));
  if (theInput > FCLZ_MASK_16)
    return ((Short) (40 + byteLeadingZerosTable[(theInput >> 16) & FCLZ_MASK_08]));
  if (theInput > FCLZ_MASK_08)
    return ((Short) (48 + byteLeadingZerosTable[(theInput >>  8) & FCLZ_MASK_08]));
  if (1)
    return ((Short) (56 + byteLeadingZerosTable[(theInput >>  0) & FCLZ_MASK_08]));
}

/******************************************/

U8 byteTrailingZerosTable[256];

// U8 lookupByteTrailingZeros (int x) { return (byteTrailingZerosTable[x]); }

U8 slow_count_trailing_zeros_in_byte (U8 the_byte)
{
  int shift;
  int count = 0;
  for (shift = 0; shift <= 7; shift++) {
    int is_zero = !((the_byte >> shift) & 1);
    if (is_zero)
      count++;
    else
      break;
  }
  return count;
}

void fillByteTrailingZerosTable(void)
{
  int j;
  for (j = 0; j < 256; j++)
    byteTrailingZerosTable[j] = slow_count_trailing_zeros_in_byte ((U8) j);    
}

Short countTrailingZerosInUnsignedLong (U64 theInput) {
  U64 tmp = theInput;
  int byte;
  int j = 0;
  for (j = 0; j < 8; j++) {
    byte = (tmp & 0xffULL);
    if (byte != 0) return ((Short) ((j << 3) + byteTrailingZerosTable[byte]));
    tmp >>= 8;
  }
  return ((Short) (64));
}

/******************************************/

double invPow2Tab[256];

void fillInvPow2Tab (void)
{
  int j;
  for (j = 0; j < 256; j++) {
    invPow2Tab[j] = pow (2.0, (-1.0 * ((double) j)));
  }
}

/******************************************/

double kxpByteLookup[256];

void fillKxpByteLookup (void) // must call fillInvPow2Tab() first
{
  int byte, col;
  for (byte = 0; byte < 256; byte++) {
    double sum = 0.0;
    for (col = 0; col < 8; col++) {
      int bit = (byte >> col) & 1;
      if (bit == 0) { // note the inverted logic
	sum += invPow2Tab[col+1]; // note the "+1"
      }
    }      
    kxpByteLookup[byte] = sum;
  }
}


/******************************************/

Long divideLongsRoundingUp (Long x, Long y) {
  assert (x >= 0 && y > 0);
  Long quotient = x / y;
  if (quotient * y == x) return (quotient);
  else return (quotient + 1);
}

Long longFloorLog2OfLong (Long x) {
  assert (x >= 1L); // throw an exception
  Long p = 0;
  Long y = 1;
 log2Loop:
  if (y == x) return (p);
  if (y  > x) return (p-1);
  p  += 1;
  y <<= 1;
  goto log2Loop;
}

/***********************************/

// returns an integer that is between 
// zero and ceiling(log_2(k))-1, inclusive

Long golombChooseNumberOfBaseBits (Long k, Long raw_count) {
  assert (k >= 1L);
  assert (raw_count >= 0L);
  Long count = (raw_count == 0L) ? 1L : raw_count;
  Long quotient = (k - count) / count; // integer division
  if (quotient == 0) return (0);
  else return (longFloorLog2OfLong(quotient));
}


/*******************************************************/
// A faster implementation of this (perhaps a byte-wise scheme)
// might speed up merging, specifically getResult() in cases
// where numCoupons is large.

Long countBitsSetInMatrix (U64 * array, Long length) {
  Long i = 0;
  U64 pattern = 0;
  Long count = 0;
// Wegner's Bit-Counting Algorithm, CACM 3 (1960), p. 322.
  for (i = 0; i < length; i++) {
    pattern = array[i];
    while (pattern != 0) { 
      pattern &= (pattern - 1); 
      count++;
    }
  }
  return count;
}

/*******************************************************/
