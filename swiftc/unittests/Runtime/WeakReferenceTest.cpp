//===--- WeakReferenceTest.cpp - Tests for weak references ---------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Runtime/ARC.h"
#include <gtest/gtest.h>

using namespace swiftc::runtime;

class TestObject : public RefCountedObject {
public:
  int value;
  TestObject(int val) : value(val) {}
};

class WeakReferenceTest : public ::testing::Test {
protected:
  void SetUp() override {
    ARCRuntime::initialize();
  }
  
  void TearDown() override {
    ARCRuntime::shutdown();
  }
};

TEST_F(WeakReferenceTest, BasicWeakReference) {
  WeakRef<TestObject> weakRef;
  
  EXPECT_TRUE(weakRef.isNil());
  EXPECT_EQ(weakRef.load(), nullptr);
  
  {
    auto strongRef = makeStrong<TestObject>(42);
    weakRef = makeWeak(strongRef);
    
    EXPECT_FALSE(weakRef.isNil());
    
    auto loaded = weakRef.load();
    EXPECT_NE(loaded, nullptr);
    EXPECT_EQ(loaded->value, 42);
    
    // Release the loaded reference
    static_cast<const RefCountedObject*>(loaded)->release();
  }
  
  // Object should be deallocated, weak reference should be nil
  EXPECT_TRUE(weakRef.isNil());
  EXPECT_EQ(weakRef.load(), nullptr);
}

TEST_F(WeakReferenceTest, WeakReferenceCopy) {
  auto strongRef = makeStrong<TestObject>(123);
  WeakRef<TestObject> weakRef1 = makeWeak(strongRef);
  
  EXPECT_FALSE(weakRef1.isNil());
  
  WeakRef<TestObject> weakRef2 = weakRef1;  // Copy constructor
  EXPECT_FALSE(weakRef2.isNil());
  
  WeakRef<TestObject> weakRef3;
  weakRef3 = weakRef1;  // Assignment operator
  EXPECT_FALSE(weakRef3.isNil());
  
  // All weak references should point to the same object
  auto loaded1 = weakRef1.load();
  auto loaded2 = weakRef2.load();
  auto loaded3 = weakRef3.load();
  
  EXPECT_EQ(loaded1, loaded2);
  EXPECT_EQ(loaded2, loaded3);
  EXPECT_EQ(loaded1->value, 123);
  
  // Release loaded references
  static_cast<const RefCountedObject*>(loaded1)->release();
  static_cast<const RefCountedObject*>(loaded2)->release();
  static_cast<const RefCountedObject*>(loaded3)->release();
}

TEST_F(WeakReferenceTest, WeakToStrongConversion) {
  WeakRef<TestObject> weakRef;
  
  {
    auto strongRef = makeStrong<TestObject>(456);
    weakRef = makeWeak(strongRef);
    
    // Convert weak to strong
    auto loaded = weakRef.load();
    EXPECT_NE(loaded, nullptr);
    
    StrongRef<TestObject> newStrongRef(loaded);
    EXPECT_EQ(newStrongRef->value, 456);
    EXPECT_EQ(strongRef->getStrongRefCount(), 2u); // Original + new strong ref
  }
  
  // Original strong reference is gone, but new one should keep object alive
  EXPECT_FALSE(weakRef.isNil());
}

TEST_F(WeakReferenceTest, WeakReferenceToNil) {
  WeakRef<TestObject> weakRef;
  
  {
    auto strongRef = makeStrong<TestObject>(789);
    weakRef = makeWeak(strongRef);
    EXPECT_FALSE(weakRef.isNil());
  } // strongRef destroyed
  
  // Object should be deallocated
  EXPECT_TRUE(weakRef.isNil());
  EXPECT_EQ(weakRef.load(), nullptr);
  
  // Multiple calls to isNil() should be safe
  EXPECT_TRUE(weakRef.isNil());
  EXPECT_TRUE(weakRef.isNil());
}

TEST_F(WeakReferenceTest, WeakReferenceReassignment) {
  auto obj1 = makeStrong<TestObject>(1);
  auto obj2 = makeStrong<TestObject>(2);
  
  WeakRef<TestObject> weakRef = makeWeak(obj1);
  EXPECT_FALSE(weakRef.isNil());
  
  auto loaded = weakRef.load();
  EXPECT_EQ(loaded->value, 1);
  static_cast<const RefCountedObject*>(loaded)->release();
  
  // Reassign to different object
  weakRef = makeWeak(obj2);
  EXPECT_FALSE(weakRef.isNil());
  
  loaded = weakRef.load();
  EXPECT_EQ(loaded->value, 2);
  static_cast<const RefCountedObject*>(loaded)->release();
}

TEST_F(WeakReferenceTest, SelfAssignment) {
  auto strongRef = makeStrong<TestObject>(999);
  WeakRef<TestObject> weakRef = makeWeak(strongRef);
  
  // Self-assignment should be safe
  weakRef = weakRef;
  
  EXPECT_FALSE(weakRef.isNil());
  auto loaded = weakRef.load();
  EXPECT_NE(loaded, nullptr);
  EXPECT_EQ(loaded->value, 999);
  
  static_cast<const RefCountedObject*>(loaded)->release();
}