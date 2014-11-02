#include "ObjCWeak.h"

void tryWeakReferencing(id (^makeThing)(void)) {
  id thingy;
  @autoreleasepool {
    thingy = makeThing();
  }

  __weak id weakThingy = thingy;

  @autoreleasepool {
    NSLog(@"before giving up strong reference:");
    id x = weakThingy;
    if (x) {
      NSLog(@"%@", [x description]);
    } else {
      NSLog(@"Gone");
    }
  }

  thingy = nil;

  @autoreleasepool {
    NSLog(@"after giving up strong reference:");
    id x = weakThingy;
    if (x) {
      NSLog(@"%@", [x description]);
    } else {
      NSLog(@"Gone");
    }
  }
}
