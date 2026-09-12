#include "cdecl.h"

#include <stdio.h>
#include <stdlib.h>

static struct Immortal theImmortal = {42};

// 'Opaque' is only forward-declared in the header; its layout is private to
// this file.
struct Opaque {
  int value;
  int refCount;
};

void retainShared(struct Shared *s) { s->refCount++; }
void releaseShared(struct Shared *s) { s->refCount--; }
void retainOpaque(struct Opaque *o) { o->refCount++; }
void releaseOpaque(struct Opaque *o) { o->refCount--; }

struct Immortal *getImmortal(void) { return &theImmortal; }

struct Shared *makeShared(int value) {
  struct Shared *s = (struct Shared *)malloc(sizeof(struct Shared));
  s->value = value;
  s->refCount = 1;
  return s;
}

int sharedRefCount(struct Shared *s) { return s->refCount; }

struct Opaque *makeOpaque(int value) {
  struct Opaque *o = (struct Opaque *)malloc(sizeof(struct Opaque));
  o->value = value;
  o->refCount = 1;
  return o;
}

int opaqueValue(struct Opaque *o) { return o->value; }
int opaqueRefCount(struct Opaque *o) { return o->refCount; }

int callSwiftImplementations(struct Shared *s, struct Opaque *o) {
  CImplTakesImmortal(getImmortal());
  printf("CImplReturnsImmortal: %d\n", CImplReturnsImmortal()->value);
  printf("CImplGetOpaqueValue: %d\n", CImplGetOpaqueValue(o));
  return CImplGetSharedValue(s);
}
