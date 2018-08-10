// Copyright 2018, Kevin Lang, Oath Research

/*
gcc -Wall -pedantic -Iinclude -Llib -o multi_reading_linsolve multi_reading_linsolve.c -lgsl -lgslcblas
*/

#include <stdio.h>
#include <assert.h>
#include <gsl/gsl_linalg.h>

/*****************************************************************************/

#ifdef EXAMPLE_FOR_INTUITION

#define NUMEQNS 5
#define NUMVARS 4
double a_data[] = 
    { 0.18, 0.60, 0.57, 0.96, 
      0.41, 0.24, 0.99, 0.58, 
      0.14, 0.30, 0.97, 0.66, 
      0.51, 0.13, 0.19, 0.85,
      0.65, 0.43, 1.16, 1.51     
    };

double b_data[] = 
  { 1.0, 2.0, 3.0, 4.0,
    7.0
 };

#endif

/*****************************************************************************/

void read_a_data (int num_eqns, int num_vars, double * a_data) {
  int i, j, nxt;
  double tmp;
  nxt = 0;
  for (i = 0; i < num_eqns; i++) {
    for (j = 0; j < num_vars; j++) {
      int got = scanf ("%lf", &tmp);
      assert (got == 1);
      a_data[nxt++] = tmp;
    }
  }
  assert (nxt == num_eqns * num_vars);
}

/*****************************************************************************/

void read_b_data (int num_eqns, double * b_data) {
  int i, nxt;
  double tmp;
  nxt = 0;
  for (i = 0; i < num_eqns; i++) {
    int got = scanf ("%lf", &tmp);
    assert (got == 1);
    b_data[nxt++] = tmp;
  }
  assert (nxt == num_eqns);
}

/*****************************************************************************/

int main (int argc, char ** argv)
{
  if (argc != 3) {
    fprintf (stderr, "Usage: %s num_eqns num_vars < data_file\n", argv[0]);
    fprintf (stderr, "    The data_file contains a matrix followed by a target vector.\n");
    fprintf (stderr, "    The matrix should be in equation major order.\n");
    fprintf (stderr, "    This version solves multiple problems with matching matrix dimensions.\n");
    fprintf (stderr, "    Each problem should be preceding by a '1'.\n");
    fprintf (stderr, "    The last problem should be followed by a '0'.\n");
    return (-1);
  }

  int num_eqns  = atoi(argv[1]);
  int num_vars  = atoi(argv[2]);

  double * a_data = (double *) malloc (num_eqns * num_vars * sizeof(double));
  assert (a_data != NULL);
  
  double * b_data = (double *) malloc (num_eqns * sizeof(double));
  assert (b_data != NULL);

  while (1) {
    int keep_going = 0;
    int got = scanf ("%d", &keep_going);
    assert (got == 1);
    if (keep_going == 0) return 0;

    read_a_data (num_eqns, num_vars, a_data);
    read_b_data (num_eqns, b_data);

    gsl_matrix_view m = gsl_matrix_view_array (a_data, num_eqns, num_vars);
    gsl_vector *x     = gsl_vector_alloc (num_vars);
    gsl_vector *tau   = gsl_vector_alloc (num_vars);
    gsl_vector *residual = gsl_vector_alloc (num_eqns);
    gsl_vector_view b = gsl_vector_view_array (b_data, num_eqns);
    int s;

    s = gsl_linalg_QR_decomp (&m.matrix, tau);
    s = gsl_linalg_QR_lssolve (&m.matrix, tau, &b.vector, x, residual);
    gsl_vector_fprintf (stdout, x, "%.15g"); fflush (stdout);
    gsl_vector_free (x);
    gsl_vector_free (tau);
    gsl_vector_free (residual);
  }

  return 0;
}


    //  printf ("residual = \n"); gsl_vector_fprintf (stdout, residual, "%g");
    //      printf("solver got A: %d %d %.19g\n", i, j, tmp);
    //    printf("solver got B: %d %.19g\n", i, tmp);
