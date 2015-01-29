//===- swift/unittests/runtime/weak.mm - Weak-pointer tests ---------------===//
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

#include <Foundation/NSObject.h>
#include <objc/runtime.h>
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "gtest/gtest.h"

using namespace swift;

// Declare some Objective-C stuff.
extern "C" void objc_release(id);

static unsigned DestroyedObjCCount = 0;

/// A trivial class that increments DestroyedObjCCount when deallocated.
@interface ObjCClass : NSObject @end
@implementation ObjCClass
- (void) dealloc {
  DestroyedObjCCount++;
  [super dealloc];
  // Clobber the object's "isa" pointer to simulate the object's memory getting
  // reclaimed.
  object_setClass(self, nullptr);
}
@end
static HeapObject *make_objc_object() {
  return (HeapObject*) [ObjCClass new];
}

// Make a Native Swift object by calling a Swift function.
// make_swift_object is defined in TestHelpers.swift as part of StdlibUnittest.
extern "C" HeapObject *make_swift_object();

static void unknown_release(void *value) {
  objc_release((id) value);
}

TEST(WeakTest, preconditions) {
  swift_release(make_swift_object());
  unknown_release(make_objc_object());
}

TEST(WeakTest, simple_swift) {
  HeapObject *o1 = make_swift_object();
  HeapObject *o2 = make_swift_object();
  ASSERT_NE(o1, o2);
  ASSERT_NE(o1, nullptr);
  ASSERT_NE(o2, nullptr);

  WeakReference ref1;
  swift_weakInit(&ref1, o1);

  HeapObject *tmp = swift_weakLoadStrong(&ref1);
  ASSERT_EQ(tmp, o1);
  swift_release(tmp);

  tmp = swift_weakLoadStrong(&ref1);
  ASSERT_EQ(o1, tmp);
  swift_release(tmp);

  swift_weakAssign(&ref1, o2);
  tmp = swift_weakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  swift_release(tmp);

  tmp = swift_weakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  swift_release(tmp);

  swift_release(o1);

  tmp = swift_weakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  swift_release(tmp);

  swift_release(o2);

  tmp = swift_weakLoadStrong(&ref1);
  ASSERT_EQ(nullptr, tmp);
  swift_release(tmp);

  swift_weakDestroy(&ref1);
}

TEST(WeakTest, simple_objc) {
  void *o1 = make_objc_object();
  void *o2 = make_objc_object();
  ASSERT_NE(o1, o2);
  ASSERT_NE(o1, nullptr);
  ASSERT_NE(o2, nullptr);

  DestroyedObjCCount = 0;
  
  WeakReference ref1;
  swift_unknownWeakInit(&ref1, o1);

  void *tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(tmp, o1);
  unknown_release(tmp);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o1, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  swift_unknownWeakAssign(&ref1, o2);
  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  unknown_release(o1);
  ASSERT_EQ(1U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(1U, DestroyedObjCCount);

  unknown_release(o2);
  ASSERT_EQ(2U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(nullptr, tmp);
  unknown_release(tmp);
  ASSERT_EQ(2U, DestroyedObjCCount);

  swift_unknownWeakDestroy(&ref1);
}

TEST(WeakTest, simple_swift_as_unknown) {
  void *o1 = make_swift_object();
  void *o2 = make_swift_object();
  ASSERT_NE(o1, o2);
  ASSERT_NE(o1, nullptr);
  ASSERT_NE(o2, nullptr);

  WeakReference ref1;
  swift_unknownWeakInit(&ref1, o1);

  void *tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(tmp, o1);
  unknown_release(tmp);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o1, tmp);
  unknown_release(tmp);

  swift_unknownWeakAssign(&ref1, o2);
  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);

  unknown_release(o1);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);

  unknown_release(o2);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(nullptr, tmp);
  unknown_release(tmp);

  swift_unknownWeakDestroy(&ref1);
}

TEST(WeakTest, simple_swift_and_objc) {
  void *o1 = make_swift_object();
  void *o2 = make_objc_object();
  ASSERT_NE(o1, o2);
  ASSERT_NE(o1, nullptr);
  ASSERT_NE(o2, nullptr);

  DestroyedObjCCount = 0;
  
  WeakReference ref1;
  swift_unknownWeakInit(&ref1, o1);

  void *tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(tmp, o1);
  unknown_release(tmp);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o1, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  swift_unknownWeakAssign(&ref1, o2);
  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  unknown_release(o1);
  ASSERT_EQ(0U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  unknown_release(o2);
  ASSERT_EQ(1U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(nullptr, tmp);
  unknown_release(tmp);
  ASSERT_EQ(1U, DestroyedObjCCount);

  swift_unknownWeakDestroy(&ref1);
}

TEST(WeakTest, simple_objc_and_swift) {
  void *o1 = make_objc_object();
  void *o2 = make_swift_object();
  ASSERT_NE(o1, o2);
  ASSERT_NE(o1, nullptr);
  ASSERT_NE(o2, nullptr);

  DestroyedObjCCount = 0;
  
  WeakReference ref1;
  swift_unknownWeakInit(&ref1, o1);

  void *tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(tmp, o1);
  unknown_release(tmp);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o1, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  swift_unknownWeakAssign(&ref1, o2);
  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(0U, DestroyedObjCCount);

  unknown_release(o1);
  ASSERT_EQ(1U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(o2, tmp);
  unknown_release(tmp);
  ASSERT_EQ(1U, DestroyedObjCCount);

  unknown_release(o2);
  ASSERT_EQ(1U, DestroyedObjCCount);

  tmp = swift_unknownWeakLoadStrong(&ref1);
  ASSERT_EQ(nullptr, tmp);
  unknown_release(tmp);
  ASSERT_EQ(1U, DestroyedObjCCount);

  swift_unknownWeakDestroy(&ref1);
}

TEST(WeakTest, objc_weak_release_after_strong_release) {
  DestroyedObjCCount = 0;

  void *o = make_objc_object();
  // strong 1, unowned 0

  swift_unknownWeakRetain(o);
  // strong 1, unowned 1

  swift_unknownRelease(o);
  // strong 0, unowned 1 -- object gets greedily deallocated by ObjC runtime
  ASSERT_EQ(1U, DestroyedObjCCount);
  
  swift_unknownWeakRelease(o);
  // strong 1, unowned 0
}
