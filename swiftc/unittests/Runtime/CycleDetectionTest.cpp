//===--- CycleDetectionTest.cpp - Tests for cycle detection --------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Runtime/ARC.h"
#include <gtest/gtest.h>

using namespace swiftc::runtime;

class CycleDetectionTest : public ::testing::Test {
protected:
  void SetUp() override {
    ARCRuntime::initialize();
    ARCRuntime::setDebugging(true);
  }
  
  void TearDown() override {
    ARCRuntime::setDebugging(false);
    ARCRuntime::shutdown();
  }
};

// Test objects for cycle detection
class Node : public RefCountedObject {
public:
  int value;
  StrongRef<Node> next;
  WeakRef<Node> weakNext;
  
  Node(int val) : value(val) {}
};

class Parent : public RefCountedObject {
public:
  int value;
  std::vector<StrongRef<Child>> children;
  
  Parent(int val) : value(val) {}
};

class Child : public RefCountedObject {
public:
  int value;
  WeakRef<Parent> parent;  // Weak reference to break cycle
  
  Child(int val) : value(val) {}
};

TEST_F(CycleDetectionTest, NoCycle) {
  auto node1 = makeStrong<Node>(1);
  auto node2 = makeStrong<Node>(2);
  auto node3 = makeStrong<Node>(3);
  
  // Linear chain: node1 -> node2 -> node3
  node1->next = node2;
  node2->next = node3;
  
  // No cycles should be detected
  ARCRuntime::checkForCycles();
  
  EXPECT_EQ(node1->getStrongRefCount(), 1u);
  EXPECT_EQ(node2->getStrongRefCount(), 1u); // Held by node1->next
  EXPECT_EQ(node3->getStrongRefCount(), 1u); // Held by node2->next
}

TEST_F(CycleDetectionTest, SimpleStrongCycle) {
  auto node1 = makeStrong<Node>(1);
  auto node2 = makeStrong<Node>(2);
  
  // Create a cycle: node1 -> node2 -> node1
  node1->next = node2;
  node2->next = node1;
  
  EXPECT_EQ(node1->getStrongRefCount(), 2u); // Held by local var + node2->next
  EXPECT_EQ(node2->getStrongRefCount(), 1u); // Held by node1->next
  
  // This would create a memory leak in a real scenario
  // The cycle detection should identify this
  ARCRuntime::checkForCycles();
}

TEST_F(CycleDetectionTest, WeakReferenceCycleBreaking) {
  auto node1 = makeStrong<Node>(1);
  auto node2 = makeStrong<Node>(2);
  
  // Create a cycle with weak reference: node1 -> node2 -weak-> node1
  node1->next = node2;
  node2->weakNext = makeWeak(node1);
  
  EXPECT_EQ(node1->getStrongRefCount(), 1u); // Only held by local var
  EXPECT_EQ(node2->getStrongRefCount(), 1u); // Held by node1->next
  
  // No strong cycle should be detected
  ARCRuntime::checkForCycles();
  
  // When node1 goes out of scope, both objects should be deallocated
}

TEST_F(CycleDetectionTest, ParentChildRelationship) {
  auto parent = makeStrong<Parent>(1);
  auto child1 = makeStrong<Child>(2);
  auto child2 = makeStrong<Child>(3);
  
  // Parent holds strong references to children
  parent->children.push_back(child1);
  parent->children.push_back(child2);
  
  // Children hold weak references to parent (breaks cycle)
  child1->parent = makeWeak(parent);
  child2->parent = makeWeak(parent);
  
  EXPECT_EQ(parent->getStrongRefCount(), 1u); // Only held by local var
  EXPECT_EQ(child1->getStrongRefCount(), 1u); // Held by parent->children
  EXPECT_EQ(child2->getStrongRefCount(), 1u); // Held by parent->children
  
  // No cycles should be detected due to weak references
  ARCRuntime::checkForCycles();
  
  // Verify weak references work
  EXPECT_FALSE(child1->parent.isNil());
  EXPECT_FALSE(child2->parent.isNil());
  
  auto loadedParent = child1->parent.load();
  EXPECT_NE(loadedParent, nullptr);
  EXPECT_EQ(loadedParent->value, 1);
  
  static_cast<const RefCountedObject*>(loadedParent)->release();
}

TEST_F(CycleDetectionTest, ComplexCycleWithWeakBreaking) {
  auto node1 = makeStrong<Node>(1);
  auto node2 = makeStrong<Node>(2);
  auto node3 = makeStrong<Node>(3);
  auto node4 = makeStrong<Node>(4);
  
  // Create a complex graph with one weak link to break cycles
  node1->next = node2;           // 1 -> 2 (strong)
  node2->next = node3;           // 2 -> 3 (strong)
  node3->next = node4;           // 3 -> 4 (strong)
  node4->weakNext = makeWeak(node1);  // 4 -weak-> 1 (breaks cycle)
  
  EXPECT_EQ(node1->getStrongRefCount(), 1u); // Only held by local var
  EXPECT_EQ(node2->getStrongRefCount(), 1u); // Held by node1->next
  EXPECT_EQ(node3->getStrongRefCount(), 1u); // Held by node2->next
  EXPECT_EQ(node4->getStrongRefCount(), 1u); // Held by node3->next
  
  ARCRuntime::checkForCycles();
  
  // All objects should be properly deallocatable when local vars go out of scope
}