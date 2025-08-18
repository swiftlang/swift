//===--- RefCountTest.cpp - Tests for reference counting -----------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Runtime/ARC.h"
#include <gtest/gtest.h>
#include <thread>
#include <atomic>

using namespace swiftc::runtime;

class RefCountTest : public ::testing::Test {
protected:
  void SetUp() override {
    ARCRuntime::initialize();
  }
  
  void TearDown() override {
    ARCRuntime::shutdown();
  }
};

TEST_F(RefCountTest, InitialRefCount) {
  RefCountedObject obj;
  
  EXPECT_EQ(obj.getStrongRefCount(), 1u);
  EXPECT_EQ(obj.getWeakRefCount(), 1u);
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Weak));
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Unowned));
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Deinitializing));
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Deallocating));
}

TEST_F(RefCountTest, RetainRelease) {
  RefCountedObject obj;
  
  EXPECT_EQ(obj.getStrongRefCount(), 1u);
  
  obj.retain();
  EXPECT_EQ(obj.getStrongRefCount(), 2u);
  
  obj.retain();
  EXPECT_EQ(obj.getStrongRefCount(), 3u);
  
  obj.release();
  EXPECT_EQ(obj.getStrongRefCount(), 2u);
  
  obj.release();
  EXPECT_EQ(obj.getStrongRefCount(), 1u);
}

TEST_F(RefCountTest, WeakRetainRelease) {
  RefCountedObject obj;
  
  EXPECT_EQ(obj.getWeakRefCount(), 1u);
  
  obj.retainWeak();
  EXPECT_EQ(obj.getWeakRefCount(), 2u);
  
  obj.retainWeak();
  EXPECT_EQ(obj.getWeakRefCount(), 3u);
  
  obj.releaseWeak();
  EXPECT_EQ(obj.getWeakRefCount(), 2u);
  
  obj.releaseWeak();
  EXPECT_EQ(obj.getWeakRefCount(), 1u);
}

TEST_F(RefCountTest, TryRetainSuccess) {
  RefCountedObject obj;
  
  EXPECT_TRUE(obj.tryRetain());
  EXPECT_EQ(obj.getStrongRefCount(), 2u);
  
  obj.release();
  EXPECT_EQ(obj.getStrongRefCount(), 1u);
}

TEST_F(RefCountTest, FlagOperations) {
  RefCountedObject obj;
  
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Weak));
  
  obj.setFlag(ReferenceFlags::Weak);
  EXPECT_TRUE(obj.hasFlag(ReferenceFlags::Weak));
  
  obj.setFlag(ReferenceFlags::Unowned);
  EXPECT_TRUE(obj.hasFlag(ReferenceFlags::Weak));
  EXPECT_TRUE(obj.hasFlag(ReferenceFlags::Unowned));
  
  obj.clearFlag(ReferenceFlags::Weak);
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Weak));
  EXPECT_TRUE(obj.hasFlag(ReferenceFlags::Unowned));
  
  obj.clearFlag(ReferenceFlags::Unowned);
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Unowned));
}

TEST_F(RefCountTest, ThreadSafeRefCounting) {
  RefCountedObject obj;
  const int numThreads = 20;
  const int operationsPerThread = 1000;
  std::atomic<int> completedThreads{0};
  
  std::vector<std::thread> threads;
  
  // Create threads that perform retain/release operations
  for (int i = 0; i < numThreads; ++i) {
    threads.emplace_back([&obj, operationsPerThread, &completedThreads]() {
      for (int j = 0; j < operationsPerThread; ++j) {
        obj.retain();
        obj.release();
      }
      completedThreads++;
    });
  }
  
  // Wait for all threads to complete
  for (auto& thread : threads) {
    thread.join();
  }
  
  EXPECT_EQ(completedThreads.load(), numThreads);
  EXPECT_EQ(obj.getStrongRefCount(), 1u); // Should return to initial state
}

TEST_F(RefCountTest, ThreadSafeWeakRefCounting) {
  RefCountedObject obj;
  const int numThreads = 20;
  const int operationsPerThread = 1000;
  
  std::vector<std::thread> threads;
  
  // Create threads that perform weak retain/release operations
  for (int i = 0; i < numThreads; ++i) {
    threads.emplace_back([&obj, operationsPerThread]() {
      for (int j = 0; j < operationsPerThread; ++j) {
        obj.retainWeak();
        obj.releaseWeak();
      }
    });
  }
  
  // Wait for all threads to complete
  for (auto& thread : threads) {
    thread.join();
  }
  
  EXPECT_EQ(obj.getWeakRefCount(), 1u); // Should return to initial state
}

TEST_F(RefCountTest, ThreadSafeFlagOperations) {
  RefCountedObject obj;
  const int numThreads = 10;
  
  std::vector<std::thread> threads;
  
  // Create threads that set and clear flags
  for (int i = 0; i < numThreads; ++i) {
    threads.emplace_back([&obj, i]() {
      ReferenceFlags flag = (i % 2 == 0) ? ReferenceFlags::Weak : ReferenceFlags::Unowned;
      
      for (int j = 0; j < 100; ++j) {
        obj.setFlag(flag);
        obj.clearFlag(flag);
      }
    });
  }
  
  // Wait for all threads to complete
  for (auto& thread : threads) {
    thread.join();
  }
  
  // Flags should be clear after all operations
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Weak));
  EXPECT_FALSE(obj.hasFlag(ReferenceFlags::Unowned));
}

TEST_F(RefCountTest, TryRetainFailure) {
  RefCountedObject* objPtr;
  
  {
    RefCountedObject obj;
    objPtr = &obj;
    
    // Set deallocating flag to simulate object being deallocated
    obj.setFlag(ReferenceFlags::Deallocating);
    
    EXPECT_FALSE(obj.tryRetain());
    EXPECT_EQ(obj.getStrongRefCount(), 1u); // Should not change
  }
}

TEST_F(RefCountTest, ImmortalObjects) {
  RefCountedObject obj;
  obj.setFlag(ReferenceFlags::Immortal);
  
  // Immortal objects should not be affected by release operations
  for (int i = 0; i < 100; ++i) {
    obj.retain();
    obj.release();
  }
  
  EXPECT_TRUE(obj.hasFlag(ReferenceFlags::Immortal));
  // Note: In a real implementation, immortal objects would have special handling
}