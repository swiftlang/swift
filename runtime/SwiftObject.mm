//===--- SwiftObject.mm - Native Swift Object root class ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This implements runtime support for bridging between Swift and Objective-C
// types in cases where they aren't trivial.
//
//===----------------------------------------------------------------------===//

#include <objc/runtime.h>
#if __has_include(<objc/objc-internal.h>)
#include <objc/objc-internal.h>
#else
extern "C" id objc_autorelease(id);
#endif
#include "Alloc.h"

using namespace swift;

struct SwiftObject_s {
  void *isa;
  long refCount;
};

@interface SwiftObject
{
  SwiftObject_s magic;
}
- (id)retain;
- (void)release;
- (id)autorelease;
- (void)dealloc;
@end

@implementation SwiftObject
- (id)retain {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_retain(SELF);
  return self;
}
- (void)release {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_release(SELF);
}
- (id)autorelease {
  return objc_autorelease(self);
}
- (void)dealloc {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_deallocObject(SELF, 0);
}
@end
