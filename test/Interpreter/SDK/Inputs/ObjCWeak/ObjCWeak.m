#include "ObjCWeak.h"
#include <objc/runtime.h>

extern id _Nullable 
objc_initWeak(id _Nullable * _Nonnull location, id _Nullable val);

void tryWeakReferencing(id (^makeThing)(void)) {
  id thingy;
  @autoreleasepool {
    thingy = [makeThing() retain];
  }
  
  id weakThingy = nil;
  objc_initWeak(&weakThingy, thingy);
  
  @autoreleasepool {
    fputs("before giving up strong reference:\n", stderr);
    id x = objc_loadWeak(&weakThingy);
    if (x) {
      fputs([[x description] UTF8String], stderr);
      fputs("\n", stderr);
    } else {
      fputs("Gone\n", stderr);
    }
  }
  
  [thingy release];
  thingy = nil;

  for (int i = 0; i < 100; i++) {
    @autoreleasepool {
      id tmp = makeThing();
      id weakTmp = nil;
      objc_initWeak(&weakTmp, tmp);
      objc_loadWeak(&weakTmp);
      objc_storeWeak(&weakTmp, nil);
    }
  }
   
  @autoreleasepool {
    fputs("after giving up strong reference:\n", stderr);
    id x = objc_loadWeak(&weakThingy);
    if (x) {
      fputs([[x description] UTF8String], stderr);
      fputs("\n", stderr);
    } else {
      fputs("Gone\n", stderr);
    }
  }
  objc_storeWeak(&weakThingy, nil);

  @autoreleasepool {
    fputs("after giving up weak reference:\n", stderr);
    id x = objc_loadWeak(&weakThingy);
    if (x) {
      fputs([[x description] UTF8String], stderr);
      fputs("\n", stderr);
    } else {
      fputs("Gone\n", stderr);
    }
  }
}
