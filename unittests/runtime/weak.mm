//===--- weak.mm - Weak-pointer tests -------------------------------------===//
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

// A fake definition of Swift runtime's WeakReference.
// This has the proper size and alignment which is all we need.
namespace swift {
class WeakReference { void *value __attribute__((unused)); };
}

// Declare some Objective-C stuff.
extern "C" void objc_release(id);

static unsigned DestroyedObjCCount = 0;

/// A trivial class that increments DestroyedObjCCount when deallocated.
@interface ObjCClass : NSObject @end
@implementation ObjCClass
- (void) dealloc {
  DestroyedObjCCount++;
  [super dealloc];
}
@end
static HeapObject *make_objc_object() {
  return (HeapObject*) [ObjCClass new];
}

// Make a Native Swift object by calling a Swift function.
// make_swift_object is defined in TestHelpers.swift as part of StdlibUnittest.
SWIFT_CC(swift) extern "C" HeapObject *make_swift_object();

static unsigned getUnownedRetainCount(HeapObject *object) {
  return swift_unownedRetainCount(object) - 1;
}

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

TEST(WeakTest, objc_unowned_basic) {
  UnownedReference ref;
  void *objc1 = make_objc_object();
  void *objc2 = make_objc_object();
  HeapObject *swift1 = make_swift_object();
  HeapObject *swift2 = make_swift_object();

  ASSERT_NE(objc1, objc2);
  ASSERT_NE(swift1, swift2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  ASSERT_EQ(0U, getUnownedRetainCount(swift2));

  void *result;

  // ref = swift1
  swift_unknownUnownedInit(&ref, swift1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);
  swift_unknownUnownedDestroy(&ref);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));

  // ref = objc1
  swift_unknownUnownedInit(&ref, objc1);
  result = swift_unknownUnownedLoadStrong(&ref);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref = objc1 (objc self transition)
  swift_unknownUnownedAssign(&ref, objc1);
  result = swift_unknownUnownedLoadStrong(&ref);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref = objc2 (objc -> objc transition)
  swift_unknownUnownedAssign(&ref, objc2);
  result = swift_unknownUnownedLoadStrong(&ref);
  ASSERT_EQ(objc2, result);
  swift_unknownRelease(result);

  // ref = swift1 (objc -> swift transition)
  swift_unknownUnownedAssign(&ref, swift1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);

  // ref = swift1 (swift self transition)
  swift_unknownUnownedAssign(&ref, swift1);
  result = swift_unknownUnownedLoadStrong(&ref);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);

  // ref = swift2 (swift -> swift transition)
  swift_unknownUnownedAssign(&ref, swift2);
  result = swift_unknownUnownedLoadStrong(&ref);
  ASSERT_EQ(swift2, result);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  ASSERT_EQ(1U, getUnownedRetainCount(swift2));
  swift_unknownRelease(result);

  // ref = objc1 (swift -> objc transition)
  swift_unknownUnownedAssign(&ref, objc1);
  result = swift_unknownUnownedLoadStrong(&ref);
  ASSERT_EQ(objc1, result);
  ASSERT_EQ(0U, getUnownedRetainCount(swift2));
  swift_unknownRelease(result);

  swift_unknownUnownedDestroy(&ref);

  swift_unknownRelease(objc1);
  swift_unknownRelease(objc2);
  swift_unknownRelease(swift1);
  swift_unknownRelease(swift2);
}

TEST(WeakTest, objc_unowned_takeStrong) {
  UnownedReference ref;
  void *objc1 = make_objc_object();
  HeapObject *swift1 = make_swift_object();

  void *result;

  // ref = objc1
  swift_unknownUnownedInit(&ref, objc1);
  result = swift_unknownUnownedTakeStrong(&ref);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref = swift1
  swift_unknownUnownedInit(&ref, swift1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedTakeStrong(&ref);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);

  swift_unknownRelease(objc1);
  swift_unknownRelease(swift1);
}

TEST(WeakTest, objc_unowned_copyInit_nil) {
  UnownedReference ref1;
  UnownedReference ref2;

  void *result;

  // ref1 = nil
  swift_unknownUnownedInit(&ref1, nullptr);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(nullptr, result);

  // ref2 = ref1 (nil -> nil)
  swift_unknownUnownedCopyInit(&ref2, &ref1);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(nullptr, result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(nullptr, result);
  swift_unknownUnownedDestroy(&ref2);
}

TEST(WeakTest, objc_unowned_copyInit_objc) {
  UnownedReference ref1;
  UnownedReference ref2;

  void *result;
  void *objc1 = make_objc_object();

  // ref1 = objc1
  swift_unknownUnownedInit(&ref1, objc1);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref2 = ref1 (objc -> objc)
  swift_unknownUnownedCopyInit(&ref2, &ref1);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);
  swift_unknownUnownedDestroy(&ref2);

  swift_unknownUnownedDestroy(&ref1);

  swift_unknownRelease(objc1);
}

TEST(WeakTest, objc_unowned_copyInit_swift) {
  UnownedReference ref1;
  UnownedReference ref2;

  void *result;

  HeapObject *swift1 = make_swift_object();
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));

  // ref1 = swift1
  swift_unknownUnownedInit(&ref1, swift1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));

  // ref2 = ref1 (swift -> swift)
  swift_unknownUnownedCopyInit(&ref2, &ref1);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  swift_unknownUnownedDestroy(&ref2);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));

  // ref2 = ref1
  // ref2 = nil
  swift_unknownUnownedCopyInit(&ref2, &ref1);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  swift_unknownUnownedAssign(&ref2, nullptr);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(nullptr, result);
  swift_unknownRelease(result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownUnownedDestroy(&ref2);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));

  swift_unknownUnownedDestroy(&ref1);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));

  swift_unknownRelease(swift1);
}

TEST(WeakTest, objc_unowned_takeInit_nil) {
  UnownedReference ref1;
  UnownedReference ref2;

  void *result;

  // ref1 = nil
  swift_unknownUnownedInit(&ref1, nullptr);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(nullptr, result);

  // ref2 = ref1 (nil -> nil)
  swift_unknownUnownedTakeInit(&ref2, &ref1);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(nullptr, result);
  swift_unknownUnownedDestroy(&ref2);
}

TEST(WeakTest, objc_unowned_takeInit_objc) {
  UnownedReference ref1;
  UnownedReference ref2;

  void *result;
  void *objc1 = make_objc_object();

  // ref1 = objc1
  swift_unknownUnownedInit(&ref1, objc1);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref2 = ref1 (objc -> objc)
  swift_unknownUnownedTakeInit(&ref2, &ref1);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);
  swift_unknownUnownedDestroy(&ref2);

  swift_unknownRelease(objc1);
}

TEST(WeakTest, objc_unowned_takeInit_swift) {
  UnownedReference ref1;
  UnownedReference ref2;

  void *result;

  HeapObject *swift1 = make_swift_object();
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));

  // ref1 = swift1
  swift_unknownUnownedInit(&ref1, swift1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));

  // ref2 = ref1 (swift -> swift)
  swift_unknownUnownedTakeInit(&ref2, &ref1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownUnownedDestroy(&ref2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));

  // ref1 = swift1
  swift_unknownUnownedInit(&ref1, swift1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));

  // ref2 = ref1
  // ref2 = nil
  swift_unknownUnownedTakeInit(&ref2, &ref1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownUnownedAssign(&ref2, nullptr);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(nullptr, result);

  swift_unknownUnownedDestroy(&ref2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));

  swift_unknownRelease(swift1);
}

TEST(WeakTest, objc_unowned_copyAssign) {
  UnownedReference ref1;
  UnownedReference ref2;
  void *objc1 = make_objc_object();
  void *objc2 = make_objc_object();
  HeapObject *swift1 = make_swift_object();
  HeapObject *swift2 = make_swift_object();

  ASSERT_NE(objc1, objc2);
  ASSERT_NE(swift1, swift2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  ASSERT_EQ(0U, getUnownedRetainCount(swift2));

  void *result;

  // ref1 = objc1
  swift_unknownUnownedInit(&ref1, objc1);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref2 = objc1
  swift_unknownUnownedInit(&ref2, objc1);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref1 = ref2 (objc self transition)
  swift_unknownUnownedCopyAssign(&ref1, &ref2);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref2 = objc2
  swift_unknownUnownedAssign(&ref2, objc2);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc2, result);
  swift_unknownRelease(result);

  // ref1 = ref2 (objc -> objc transition)
  swift_unknownUnownedCopyAssign(&ref1, &ref2);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc2, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc2, result);
  swift_unknownRelease(result);

  // ref2 = swift1
  swift_unknownUnownedAssign(&ref2, swift1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);

  // ref1 = ref2 (objc -> swift transition)
  swift_unknownUnownedCopyAssign(&ref1, &ref2);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);

  // ref2 = swift1
  swift_unknownUnownedAssign(&ref2, swift1);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));

  // ref1 = ref2 (swift self transition)
  swift_unknownUnownedCopyAssign(&ref1, &ref2);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);

  // ref2 = swift2
  swift_unknownUnownedAssign(&ref2, swift2);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  ASSERT_EQ(1U, getUnownedRetainCount(swift2));
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift2, result);
  swift_unknownRelease(result);

  // ref1 = ref2 (swift -> swift transition)
  swift_unknownUnownedCopyAssign(&ref1, &ref2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  ASSERT_EQ(2U, getUnownedRetainCount(swift2));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift2, result);
  ASSERT_EQ(2U, getUnownedRetainCount(swift2));
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift2, result);
  ASSERT_EQ(2U, getUnownedRetainCount(swift2));
  swift_unknownRelease(result);

  // ref2 = objc1
  swift_unknownUnownedAssign(&ref2, objc1);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc1, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift2));
  swift_unknownRelease(result);

  // ref1 = ref2 (swift -> objc transition)
  swift_unknownUnownedCopyAssign(&ref1, &ref2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  ASSERT_EQ(0U, getUnownedRetainCount(swift2));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  swift_unknownUnownedDestroy(&ref1);
  swift_unknownUnownedDestroy(&ref2);

  swift_unknownRelease(objc1);
  swift_unknownRelease(objc2);
  swift_unknownRelease(swift1);
  swift_unknownRelease(swift2);
}

TEST(WeakTest, objc_unowned_takeAssign) {
  UnownedReference ref1;
  UnownedReference ref2;
  void *objc1 = make_objc_object();
  void *objc2 = make_objc_object();
  HeapObject *swift1 = make_swift_object();
  HeapObject *swift2 = make_swift_object();

  ASSERT_NE(objc1, objc2);
  ASSERT_NE(swift1, swift2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  ASSERT_EQ(0U, getUnownedRetainCount(swift2));

  void *result;

  // ref1 = objc1
  swift_unknownUnownedInit(&ref1, objc1);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref2 = objc1
  swift_unknownUnownedInit(&ref2, objc1);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref1 = ref2 (objc self transition)
  swift_unknownUnownedTakeAssign(&ref1, &ref2);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  // ref2 = objc2
  swift_unknownUnownedInit(&ref2, objc2);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc2, result);
  swift_unknownRelease(result);

  // ref1 = ref2 (objc -> objc transition)
  swift_unknownUnownedTakeAssign(&ref1, &ref2);
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc2, result);
  swift_unknownRelease(result);

  // ref2 = swift1
  swift_unknownUnownedInit(&ref2, swift1);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);

  // ref1 = ref2 (objc -> swift transition)
  swift_unknownUnownedTakeAssign(&ref1, &ref2);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);

  // ref2 = swift1
  swift_unknownUnownedInit(&ref2, swift1);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift1, result);
  swift_unknownRelease(result);
  ASSERT_EQ(2U, getUnownedRetainCount(swift1));

  // ref1 = ref2 (swift self transition)
  swift_unknownUnownedTakeAssign(&ref1, &ref2);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift1, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  swift_unknownRelease(result);

  // ref2 = swift2
  swift_unknownUnownedInit(&ref2, swift2);
  ASSERT_EQ(1U, getUnownedRetainCount(swift1));
  ASSERT_EQ(1U, getUnownedRetainCount(swift2));
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(swift2, result);
  swift_unknownRelease(result);

  // ref1 = ref2 (swift -> swift transition)
  swift_unknownUnownedTakeAssign(&ref1, &ref2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  ASSERT_EQ(1U, getUnownedRetainCount(swift2));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(swift2, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift2));
  swift_unknownRelease(result);

  // ref2 = objc1
  swift_unknownUnownedInit(&ref2, objc1);
  result = swift_unknownUnownedLoadStrong(&ref2);
  ASSERT_EQ(objc1, result);
  ASSERT_EQ(1U, getUnownedRetainCount(swift2));
  swift_unknownRelease(result);

  // ref1 = ref2 (swift -> objc transition)
  swift_unknownUnownedTakeAssign(&ref1, &ref2);
  ASSERT_EQ(0U, getUnownedRetainCount(swift1));
  ASSERT_EQ(0U, getUnownedRetainCount(swift2));
  result = swift_unknownUnownedLoadStrong(&ref1);
  ASSERT_EQ(objc1, result);
  swift_unknownRelease(result);

  swift_unknownUnownedDestroy(&ref1);

  swift_unknownRelease(objc1);
  swift_unknownRelease(objc2);
  swift_unknownRelease(swift1);
  swift_unknownRelease(swift2);
}
