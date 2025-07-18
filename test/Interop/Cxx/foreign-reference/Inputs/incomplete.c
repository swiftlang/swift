#include <stdlib.h>
#include <stdio.h>

#include "reference-counted.h"

typedef struct IncompleteImpl {
  unsigned refCount;
  double weight;
} *Incomplete;


Incomplete Incomplete_create(double weight) {
  Incomplete result = (Incomplete)malloc(sizeof(IncompleteImpl));
  result->refCount = 1;
  result->weight = weight;
  return result;
}

void INRetain(Incomplete i) {
  i->refCount++;
}

void INRelease(Incomplete i) {
  i->refCount--;
  if (i->refCount == 0) {
    printf("Destroyed instance containing weight %f\n", i->weight);
    free(i);
  }
}

double Incomplete_getWeight(Incomplete i) {
  return i->weight;
}
