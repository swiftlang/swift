//===--- NSSwiftArrayBase.mm - Native Swift Array base class --------------===//
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
// arrays.
//
//===----------------------------------------------------------------------===//

#import <Foundation/Foundation.h>
#include <objc/NSObject.h>
#include <objc/runtime.h>
#if __has_include(<objc/objc-internal.h>)
#include <objc/objc-internal.h>
#endif
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/ObjCBridge.h"
#include "llvm/ADT/DenseMap.h"
#include "Private.h"
#include <stdio.h>
#include <stdlib.h>
#include <mutex>
#include "../shims/shims.h"

// Redeclare these just we check them.
extern "C" id _objc_rootAutorelease(id);

using namespace swift;

@interface NSSwiftArrayBase : NSArray
{
  uint32_t magic1;
  uint32_t magic2;
}
@end

@implementation NSSwiftArrayBase
+ (void)load {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  class_setSuperclass(objc_getClass("NSSwiftArray"), [NSSwiftArrayBase class]);
#pragma clang diagnostic pop
}

- (id)retain {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_retain(SELF);
  return self;
}
- (oneway void)release {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_release(SELF);
}
- (id)autorelease {
  return _objc_rootAutorelease(self);
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-missing-super-calls"
- (void)dealloc {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_deallocObject(SELF, 0);
}
#pragma clang diagnostic pop

- (bool) __usesNativeSwiftReferenceCounting {
  return true;
}
@end
