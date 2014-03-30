/*
  C-program to solve the two-dimensional Poisson equation on 
  a unit square using one-dimensional eigenvalue decompositions
  and fast sine transforms

  einar m. ronquist
  ntnu, october 2000
  revised, october 2001
*/

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <math.h>

#include <mpi.h>

/* function prototypes */
double *createdoubleArray (int n);
double **createdouble2DArray (int m, int n);
void freedouble2DArray (double **array);

void block_transpose (double **bt, double **b, int m, int m_loc, int size, int rank);
void block_copy (double **from, double **to, int m, int offset);
void transpose (double **bt, double **b, int m, int offset);
void fst_(double *v, int *n, double *w, int *nn);
void fstinv_(double *v, int *n, double *w, int *nn);

/* global variables <3 */
static int m_loc, m_padded;
static MPI_Datatype block_type;

int main(int argc, char **argv )
{
  double *diag, **b, **bt, *sendbuf, *recvbuf, *z;
  double pi, h, umax;
  int i, j, n, m, nn;

  /* the total number of grid points in each spatial direction is (n+1) */
  /* the total number of degrees-of-freedom in each spatial direction is (n-1) */
  /* this version requires n to be a power of 2 */

 if( argc < 2 ) {
    printf("need a problem size\n");
    return 1;
  }

  n  = atoi(argv[1]);
  m  = n-1;
  nn = 4*n;

  int size, rank;
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  int m_loc = m / size;
  if (m % size) {
    m_loc++;
    m_padded = m_loc * size;
  } else {
    m_padded = m;
  }

  MPI_Type_vector(m_loc, m_loc,
		  m_padded, MPI_DOUBLE, &block_type);
  MPI_Type_commit(&block_type);

  diag = createdoubleArray (m);
  b    = createdouble2DArray (m_loc,m_padded);
  bt   = createdouble2DArray (m_loc,m_padded);
  z    = createdoubleArray (nn);

  h    = 1./(double)n;
  pi   = 4.*atan(1.);

  for (i=0; i < m; i++) {
    diag[i] = 2.*(1.-cos((i+1)*pi/(double)n));
  }
  for (j=0; j < m_loc; j++) {
    for (i=0; i < m; i++) {
      b[j][i] = h*h;
    }
  }
  for (j=0; j < m_loc; j++) {
    fst_(b[j], &n, z, &nn);
  }

  block_transpose (bt, b, m_padded, m_loc, size, rank);

  for (i=0; i < m_loc; i++) {
    fstinv_(bt[i], &n, z, &nn);
  }
  
  // TODO: Some shit here
  for (j=0; j < m_loc; j++) {
    for (i=0; i < m; i++) {
      bt[j][i] = bt[j][i]/(diag[i]+diag[j + rank*m_loc]);
    }
  }
  
  for (i=0; i < m_loc; i++) {
    fst_(bt[i], &n, z, &nn);
  }

  block_transpose (b, bt, m_padded, m_loc, size, rank);

  for (j=0; j < m_loc; j++) {
    fstinv_(b[j], &n, z, &nn);
  }

  umax = 0.0;
  for (j=0; j < m_loc; j++) {
    for (i=0; i < m; i++) {
      if (b[j][i] > umax) umax = b[j][i];
    }
  }
  printf (" umax = %e \n",umax);

  MPI_Finalize();

  return 0;
}

void block_transpose (double **bt, double **b, int m, int m_loc, int size, int rank)
{
  MPI_Alltoall(*b, 1, block_type, *bt, 1, block_type, MPI_COMM_WORLD);
  
  double **temp_block = createdouble2DArray (m_loc, m_loc);
  int i;
  for (i = 0; i < size; i++) {
    transpose(temp_block, bt, m_loc, m_loc*i);
    block_copy(temp_block, bt, m_loc, m_loc*i);
  }
  freedouble2DArray (temp_block);
}

void block_copy (double **from, double **to, int m, int offset)
{
  int i,j;
  for (j=0; j < m; j++) {
    for (i=0; i < m; i++) {
      to[j][i+offset] = from[j][i];
    }
  }
}

void transpose (double **bt, double **b, int m, int offset)
{
  int i, j;
  for (j=0; j < m; j++) {
    for (i=0; i < m; i++) {
      bt[j][i] = b[i][j+offset];
    }
  }
}

double *createdoubleArray (int n)
{
  double *a;
  int i;
  a = (double *)malloc(n*sizeof(double));
  for (i=0; i < n; i++) {
    a[i] = 0.0;
  }
  return (a);
}

double **createdouble2DArray (int n1, int n2)
{
  int i, n;
  double **a;
  a    = (double **)malloc(n1   *sizeof(double *));
  a[0] = (double  *)malloc(n1*n2*sizeof(double));
  for (i=1; i < n1; i++) {
    a[i] = a[i-1] + n2;
  }
  n = n1*n2;
  memset(a[0],0,n*sizeof(double));
  return (a);
}

void freedouble2DArray (double **array)
{
  free(array[0]);
  free(array);
}
