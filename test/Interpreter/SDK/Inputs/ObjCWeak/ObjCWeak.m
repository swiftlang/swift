#include "ObjCWeak.h"

void tryWeakReferencing(id (^makeThing)(void)) {
  id thingy;
  @autoreleasepool {
    thingy = makeThing();
  }

  __weak id weakThingy = thingy;

  @autoreleasepool {
    fputs("before giving up strong reference:\n", stderr);
    id x = weakThingy;
    if (x) {
      fputs([[x description] UTF8String], stderr);
      fputs("\n", stderr);
    } else {
      fputs("Gone\n", stderr);
    }
  }

  thingy = nil;

  @autoreleasepool {
    fputs("after giving up strong reference:\n", stderr);
    id x = weakThingy;
    if (x) {
      fputs([[x description] UTF8String], stderr);
      fputs("\n", stderr);
    } else {
      fputs("Gone\n", stderr);
    }
  }
}
