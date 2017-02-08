//===--- Refcounting.mm - Reference-counting for ObjC ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <Foundation/NSObject.h>
#include <objc/runtime.h>
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "gtest/gtest.h"

using namespace swift;

static unsigned DestroyedObjCCount = 0;
/// A trivial class that increments DestroyedObjCCount when deallocated.
@interface ObjCTestClass : NSObject @end
@implementation ObjCTestClass
- (void) dealloc {
  DestroyedObjCCount++;
  [super dealloc];
}
@end
static HeapObject *make_objc_object() {
  return static_cast<HeapObject *>([ObjCTestClass new]);
}

TEST(RefcountingTest, objc_unknown_retain_release_n) {
  auto object = make_objc_object();
  swift_unknownRetain_n(object, 32);
  swift_unknownRetain(object);
  swift_unknownRelease_n(object, 31);
  swift_unknownRelease(object);
  swift_unknownRelease_n(object, 1);
  swift_unknownRelease(object);
  // The object should be destroyed by now.
  EXPECT_EQ(1u, DestroyedObjCCount);
}
