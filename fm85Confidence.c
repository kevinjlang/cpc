// Copyright 2018, Kevin Lang, Oath Research

/*
  This test of merging is less exhaustive than testAll.c, 
  but is more practical for large values of K.

  gcc -O3 -Wall -pedantic -o quickTestMerge u32Table.c fm85Util.c fm85.c iconEstimator.c fm85Compression.c fm85Merging.c fm85Testing.c quickTestMerge.c

*/

/*******************************************************/

#include "common.h"
// #include "fm85Util.h"
// #include "u32Table.h"
#include "fm85.h"
#include "iconEstimator.h"
// #include "fm85Compression.h"
// #include "fm85Merging.h"

/*******************************************************/
// Note: the following empirical measurements were initially done at N = 100 * K,
// which wasn't far enough out to get really close to asymptotic values like 0.6931.
// We are currently re-running at N = 1000 * K, which the arxiv simulations show
// is nearly far enough. For example, the file fast-12.plot shows:
//    n / k             icon            HIP
//  100.806152344	0.686538483709	0.580445401405
// 1001.39941406	0.692138078763	0.587899629123

//  BTW, the file icon-table-96-lg10 contains the lines
//   7144	102396.779694706565351
//  10546	1024210.113477294100448
//  so when  n/k =  100, C/k ~=  6.9765625
//  and when n/k = 1000, C/K ~= 10.298828125.

/*******************************************************/
// ln 2.0
double iconErrorConstant = 0.693147180559945286;

//  1,    2,    3, // kappa
Short iconLowSideData [33] = {   // Empirically measured at N = 1000 * K.
 6037, 5720, 5328, // 4 1000000
 6411, 6262, 5682, // 5 1000000
 6724, 6403, 6127, // 6 1000000
 6665, 6411, 6208, // 7 1000000
 6959, 6525, 6427, // 8 1000000
 6892, 6665, 6619, // 9 1000000
 6792, 6752, 6690, // 10 1000000
 6899, 6818, 6708, // 11 1000000
 6871, 6845, 6812, // 12 1046369
 6909, 6861, 6828, // 13 1043411
 6919, 6897, 6842, // 14 1000297
};                // lgK numtrials

//  1,    2,    3, // kappa
Short iconHighSideData [33] = {   // Empirically measured at N = 1000 * K.
 8031, 8559, 9309, // 4 1000000
 7084, 7959, 8660, // 5 1000000
 7141, 7514, 7876, // 6 1000000
 7458, 7430, 7572, // 7 1000000
 6892, 7141, 7497, // 8 1000000
 6889, 7132, 7290, // 9 1000000
 7075, 7118, 7185, // 10 1000000
 7040, 7047, 7085, // 11 1000000
 6993, 7019, 7053, // 12 1046369
 6953, 7001, 6983, // 13 1043411
 6944, 6966, 7004, // 14 1000297
};                // lgK numtrials

/*******************************************************/
// sqrt((ln 2.0) / 2.0)
double hipErrorConstant = 0.588705011257737332;

//  1,    2,    3, // kappa
Short hipLowSideData [33] = {   // Empirically measured at N = 1000 * K.
 5871, 5247, 4826, // 4 1000000
 5877, 5403, 5070, // 5 1000000
 5873, 5533, 5304, // 6 1000000
 5878, 5632, 5464, // 7 1000000
 5874, 5690, 5564, // 8 1000000
 5880, 5745, 5619, // 9 1000000
 5875, 5784, 5701, // 10 1000000
 5866, 5789, 5742, // 11 1000000
 5869, 5827, 5784, // 12 1046369
 5876, 5860, 5827, // 13 1043411
 5881, 5853, 5842, // 14 1000297
};                // lgK numtrials

//  1,    2,    3, // kappa
Short hipHighSideData [33] = {   // Empirically measured at N = 1000 * K.
 5855, 6688, 7391, // 4 1000000
 5886, 6444, 6923, // 5 1000000
 5885, 6254, 6594, // 6 1000000
 5889, 6134, 6326, // 7 1000000
 5900, 6072, 6203, // 8 1000000
 5875, 6005, 6089, // 9 1000000
 5871, 5980, 6040, // 10 1000000
 5889, 5941, 6015, // 11 1000000
 5871, 5926, 5973, // 12 1046369
 5866, 5901, 5915, // 13 1043411
 5880, 5914, 5953, // 14 1000297
};                // lgK numtrials

/*******************************************************/

double getIconConfidenceLB (FM85 * sketch, int kappa) {
  if (sketch->numCoupons == 0) return 0.0;
  int lgK = sketch->lgK;
  long k = (1LL << lgK);
  assert (lgK >= 4);
  assert (kappa >= 1 && kappa <= 3);
  double x = iconErrorConstant;
  if (lgK < 14) x = ((double) iconHighSideData[3*(lgK-4) + (kappa-1)]) / 10000.0;
  double rel = x / (sqrt((double) k));
  double eps = ((double) kappa) * rel;
  double est = getIconEstimate (lgK, sketch->numCoupons);
  double result = est / (1.0 + eps);
  double check = (double) sketch->numCoupons;
  if (result < check) result = check;
  return (result);
}

double getIconConfidenceUB (FM85 * sketch, int kappa) {
  if (sketch->numCoupons == 0) return 0.0;
  int lgK = sketch->lgK;
  long k = (1LL << lgK);
  assert (lgK >= 4);
  assert (kappa >= 1 && kappa <= 3);
  double x = iconErrorConstant;
  if (lgK < 14) x = ((double) iconLowSideData[3*(lgK-4) + (kappa-1)]) / 10000.0;
  double rel = x / (sqrt((double) k));
  double eps = ((double) kappa) * rel;
  double est = getIconEstimate (lgK, sketch->numCoupons);
  double result = est / (1.0 - eps);
  return (ceil(result));  // widening for coverage
}

/*******************************************************/

double getHIPConfidenceLB (FM85 * sketch, int kappa) {
  if (sketch->numCoupons == 0) return 0.0;
  int lgK = sketch->lgK;
  long k = (1LL << lgK);
  assert (lgK >= 4);
  assert (kappa >= 1 && kappa <= 3);
  double x = hipErrorConstant;
  if (lgK < 14) x = ((double) hipHighSideData[3*(lgK-4) + (kappa-1)]) / 10000.0;
  double rel = x / (sqrt((double) k));
  double eps = ((double) kappa) * rel;
  double est = getHIPEstimate (sketch);
  double result = est / (1.0 + eps);
  double check = (double) sketch->numCoupons;
  if (result < check) result = check;
  return (result);
}

double getHIPConfidenceUB (FM85 * sketch, int kappa) {
  if (sketch->numCoupons == 0) return 0.0;
  int lgK = sketch->lgK;
  long k = (1LL << lgK);
  assert (lgK >= 4);
  assert (kappa >= 1 && kappa <= 3);
  double x = hipErrorConstant;
  if (lgK < 14) x = ((double) hipLowSideData[3*(lgK-4) + (kappa-1)]) / 10000.0;
  double rel = x / (sqrt((double) k));
  double eps = ((double) kappa) * rel;
  double est = getHIPEstimate (sketch);
  double result = est / (1.0 - eps);
  return (ceil(result)); // widening for coverage
}

/*******************************************************/

