// Copyright 2018, Kevin Lang, Oath Research

#ifndef GOT_FM85_CONFIDENCE_H

#include "common.h"
#include "fm85.h"

double getIconConfidenceLB (FM85 * sketch, int kappa);
double getIconConfidenceUB (FM85 * sketch, int kappa);
double getHIPConfidenceLB  (FM85 * sketch, int kappa);
double getHIPConfidenceUB  (FM85 * sketch, int kappa);

#define GOT_FM85_CONFIDENCE_H
#endif
