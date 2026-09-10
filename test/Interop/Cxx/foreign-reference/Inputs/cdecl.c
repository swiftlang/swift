#include "cdecl.h"

#include <stdio.h>
#include <stdlib.h>

static struct Immortal theImmortal = {42};

void retainShared(struct Shared *s) { s->refCount++; }
void releaseShared(struct Shared *s) { s->refCount--; }

struct Immortal *getImmortal(void) { return &theImmortal; }

struct Shared *makeShared(int value) {
  struct Shared *s = (struct Shared *)malloc(sizeof(struct Shared));
  s->value = value;
  s->refCount = 1;
  return s;
}

int sharedRefCount(struct Shared *s) { return s->refCount; }

int callSwiftImplementations(struct Shared *s) {
  CImplTakesImmortal(getImmortal());
  printf("CImplReturnsImmortal: %d\n", CImplReturnsImmortal()->value);
  return CImplGetSharedValue(s);
}
