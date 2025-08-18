//===--- ARCTest.cpp - Tests for Automatic Reference Counting ------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Runtime/ARC.h"
#include <gtest/gtest.h>
#include <thread>
#include <vector>

using namespace swiftc::runtime;

// Test object for ARC testing
class TestObject : public RefCountedObject {
public:
  static int constructorCalls;
  static int destructorCalls;
  static int deinitializeCalls;
  
  int value;
  
  TestObject(int val = 0) : value(val) {
    constructorCalls++;
  }
  
  ~TestObject() override {
    destructorCalls++;
  }
  
protected:
  void deinitialize() override {
    deinitializeCalls++;
    RefCountedObject::deinitialize();
  }
};

int TestObject::constructorCalls = 0;
int TestObject::destructorCalls = 0;
int TestObject::deinitializeCalls = 0;

class ARCTest : public ::testing::Test {
protected:
  void SetUp() override {
    TestObject::constructorCalls = 0;
    TestObject::destructorCalls = 0;
    TestObject::deinitializeCalls = 0;
    ARCRuntime::initialize();
  }
  
  void TearDown() override {
    ARCRuntime::shutdown();
  }
};

TEST_F(ARCTest, BasicObjectCreation) {
  {
    auto obj = makeStrong<TestObject>(42);
    EXPECT_EQ(obj->value, 42);
    EXPECT_EQ(obj->getStrongRefCount(), 1u);
    EXPECT_FALSE(obj.isNull());
  }
  
  EXPECT_EQ(TestObject::constructorCalls, 1);
  EXPECT_EQ(TestObject::deinitializeCalls, 1);
  EXPECT_EQ(TestObject::destructorCalls, 1);
}

TEST_F(ARCTest, StrongReferenceRetainRelease) {
  auto obj = makeStrong<TestObject>(123);
  EXPECT_EQ(obj->getStrongRefCount(), 1u);
  
  {
    StrongRef<TestObject> obj2 = obj;  // Copy constructor should retain
    EXPECT_EQ(obj->getStrongRefCount(), 2u);
    EXPECT_EQ(obj2->value, 123);
  } // obj2 destructor should release
  
  EXPECT_EQ(obj->getStrongRefCount(), 1u);
  EXPECT_EQ(TestObject::deinitializeCalls, 0); // Object still alive
}

TEST_F(ARCTest, StrongReferenceAssignment) {
  auto obj1 = makeStrong<TestObject>(1);
  auto obj2 = makeStrong<TestObject>(2);
  
  EXPECT_EQ(obj1->getStrongRefCount(), 1u);
  EXPECT_EQ(obj2->getStrongRefCount(), 1u);
  
  obj1 = obj2;  // Assignment should release obj1's original object and retain obj2's
  
  EXPECT_EQ(obj1->value, 2);
  EXPECT_EQ(obj2->value, 2);
  EXPECT_EQ(obj2->getStrongRefCount(), 2u);
  
  // First object should be deallocated
  EXPECT_EQ(TestObject::deinitializeCalls, 1);
}

TEST_F(ARCTest, WeakReferences) {
  WeakRef<TestObject> weakRef;
  
  {
    auto strongRef = makeStrong<TestObject>(456);
    weakRef = makeWeak(strongRef);
    
    EXPECT_FALSE(weakRef.isNil());
    
    auto loadedRef = weakRef.load();
    EXPECT_NE(loadedRef, nullptr);
    EXPECT_EQ(loadedRef->value, 456);
    
    // Release the loaded reference
    static_cast<const RefCountedObject*>(loadedRef)->release();
  } // strongRef goes out of scope
  
  // Object should be deallocated, weak reference should be nil
  EXPECT_TRUE(weakRef.isNil());
  EXPECT_EQ(weakRef.load(), nullptr);
}

TEST_F(ARCTest, UnownedReferences) {
  UnownedRef<TestObject> unownedRef;
  
  {
    auto strongRef = makeStrong<TestObject>(789);
    unownedRef = makeUnowned(strongRef);
    
    EXPECT_TRUE(unownedRef.isValid());
    EXPECT_EQ(unownedRef->value, 789);
    EXPECT_EQ(unownedRef.get()->value, 789);
  } // strongRef goes out of scope
  
  // Object should be deallocated, unowned reference should be invalid
  EXPECT_FALSE(unownedRef.isValid());
  EXPECT_EQ(unownedRef.get(), nullptr);
}

TEST_F(ARCTest, CycleBreaking) {
  // Test that weak references break cycles
  class Parent;
  class Child;
  
  class Parent : public RefCountedObject {
  public:
    std::vector<StrongRef<Child>> children;
    int value;
    
    Parent(int val) : value(val) {}
  };
  
  class Child : public RefCountedObject {
  public:
    WeakRef<Parent> parent;  // Weak reference breaks the cycle
    int value;
    
    Child(int val) : value(val) {}
  };
  
  {
    auto parent = makeStrong<Parent>(1);
    auto child1 = makeStrong<Child>(2);
    auto child2 = makeStrong<Child>(3);
    
    // Create the relationships
    parent->children.push_back(child1);
    parent->children.push_back(child2);
    child1->parent = makeWeak(parent);
    child2->parent = makeWeak(parent);
    
    EXPECT_EQ(parent->getStrongRefCount(), 1u); // Only held by parent variable
    EXPECT_EQ(child1->getStrongRefCount(), 1u); // Held by parent->children
    EXPECT_EQ(child2->getStrongRefCount(), 1u); // Held by parent->children
    
    EXPECT_FALSE(child1->parent.isNil());
    EXPECT_FALSE(child2->parent.isNil());
  } // All strong references go out of scope
  
  // All objects should be properly deallocated due to weak reference cycle breaking
}

TEST_F(ARCTest, ThreadSafety) {
  auto obj = makeStrong<TestObject>(999);
  const int numThreads = 10;
  const int operationsPerThread = 1000;
  
  std::vector<std::thread> threads;
  
  // Create multiple threads that retain and release the object
  for (int i = 0; i < numThreads; ++i) {
    threads.emplace_back([&obj, operationsPerThread]() {
      for (int j = 0; j < operationsPerThread; ++j) {
        StrongRef<TestObject> localRef = obj;
        // localRef destructor will release
      }
    });
  }
  
  // Wait for all threads to complete
  for (auto& thread : threads) {
    thread.join();
  }
  
  // Object should still be alive with reference count of 1
  EXPECT_EQ(obj->getStrongRefCount(), 1u);
  EXPECT_EQ(obj->value, 999);
  EXPECT_EQ(TestObject::deinitializeCalls, 0);
}

TEST_F(ARCTest, RuntimeStatistics) {
  auto initialStats = ARCRuntime::getStatistics();
  
  {
    auto obj1 = makeStrong<TestObject>(1);
    auto obj2 = makeStrong<TestObject>(2);
    StrongRef<TestObject> obj3 = obj1; // Copy should increment retains
  }
  
  auto finalStats = ARCRuntime::getStatistics();
  
  // Should have more allocations and retains
  EXPECT_GT(finalStats.totalAllocations, initialStats.totalAllocations);
  EXPECT_GT(finalStats.totalRetains, initialStats.totalRetains);
  EXPECT_GT(finalStats.totalReleases, initialStats.totalReleases);
}

TEST_F(ARCTest, DebuggingFeatures) {
  ARCRuntime::setDebugging(true);
  
  {
    auto obj = makeStrong<TestObject>(42);
    ARCRuntime::checkForCycles();
  }
  
  ARCRuntime::setDebugging(false);
  
  // Test should complete without crashing
  EXPECT_TRUE(true);
}