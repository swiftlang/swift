#include <stdio.h>

static int made = 0;
static int dtors = 0;
static int doubleFrees = 0;

extern "C" void noteMade(void) { made++; }
extern "C" void noteDtor(void) { dtors++; }
extern "C" void noteDoubleFree(void) { doubleFrees++; }

/// A correct program destroys exactly as many objects as it made, and never
/// destroys the same object twice.
extern "C" void reportCounts(void) {
  printf("balanced=%s doubleFree=%d\n", made == dtors ? "yes" : "no",
         doubleFrees);
}
