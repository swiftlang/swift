// Executable end-to-end test: a C++ program calls functions declared in
// foreign-reference.h that take and return foreign reference types, and
// methods of one, whose bodies are provided in Swift via
// `@cxx @implementation`, and observes their effect on the object and on the
// reference counts.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/foreign-reference-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -Xfrontend -disable-availability-checking \
// RUN:   -module-name ForeignReferenceExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/foreign-reference-execution-main.o \
// RUN:   %S/Inputs/foreign-reference-execution.swift \
// RUN:   -o %t/foreign-reference-execution
// RUN: %target-codesign %t/foreign-reference-execution
// RUN: %target-run %t/foreign-reference-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "foreign-reference.h"

// Retains minus releases.
static int liveNodes = 0;
static int liveLeaves = 0;

void retainNode(Node *) { ++liveNodes; }
void releaseNode(Node *) { --liveNodes; }
void retainLeaf(Leaf *) { ++liveLeaves; }
void releaseLeaf(Leaf *) { --liveLeaves; }

int main() {
  Node node{42};

  // A parameter is borrowed: no net retain.
  int takesNodeRes = takesNode(&node);
  printf("takesNode=%d live=%d\n", takesNodeRes, liveNodes);
  // CHECK: takesNode=42 live=0

  int takesNullableNodeRes = takesNullableNode(nullptr);
  int takesNullableNodeRes2 = takesNullableNode(&node);
  printf("takesNullableNode=%d %d live=%d\n", takesNullableNodeRes,
         takesNullableNodeRes2, liveNodes);
  // CHECK: takesNullableNode=-1 42 live=0

  // A reference to a pointer: the body reseats the caller's pointer,
  // retaining the new referent and releasing the old one.
  Node other{7};
  Node *slot = &node;
  reseatNode(slot, &other);
  int readNodePtrRes = readNodePtr(slot);
  printf("reseatNode=%d readNodePtr=%d live=%d\n", slot == &other,
         readNodePtrRes, liveNodes);
  // CHECK: reseatNode=1 readNodePtr=7 live=0

  // A retained result is returned at +1.
  Node *returned = returnsRetainedNode(&node);
  printf("returnsRetainedNode=%d live=%d\n", returned->value, liveNodes);
  // CHECK: returnsRetainedNode=42 live=1
  releaseNode(returned);

  Node *null = returnsNullableRetainedNode(&node, 1);
  printf("returnsNullableRetainedNode(null)=%d live=%d\n", null == nullptr,
         liveNodes);
  // CHECK: returnsNullableRetainedNode(null)=1 live=0

  Node *nonNull = returnsNullableRetainedNode(&node, 0);
  printf("returnsNullableRetainedNode=%d live=%d\n", nonNull->value, liveNodes);
  // CHECK: returnsNullableRetainedNode=42 live=1
  releaseNode(nonNull);

  Node *passedThrough = Node::passThrough(&node);
  printf("passThrough=%d live=%d\n", passedThrough->value, liveNodes);
  // CHECK: passThrough=42 live=1
  releaseNode(passedThrough);

  // `self` is the receiver: a const method reads it and a non-const method
  // mutates it, and neither retains it on net.
  printf("get=%d live=%d\n", node.get(), liveNodes);
  // CHECK: get=42 live=0

  node.add(8);
  printf("add=%d live=%d\n", node.value, liveNodes);
  // CHECK: add=50 live=0

  // Same-arity overloads are told apart by parameter type.
  printf("overloadedByType=%d %.1f\n", node.overloadedByType(1),
         node.overloadedByType(0.5));
  // CHECK: overloadedByType=51 50.5

  Leaf leaf{7};
  Leaf *returnedLeaf = returnsRetainedLeaf(&leaf);
  printf("returnsRetainedLeaf=%d live=%d\n", returnedLeaf->value, liveLeaves);
  // CHECK: returnsRetainedLeaf=7 live=1
  releaseLeaf(returnedLeaf);

  Singleton singleton{9};
  printf("returnsSingleton=%d\n", returnsSingleton(&singleton)->value);
  // CHECK: returnsSingleton=9

  printf("live=%d %d\n", liveNodes, liveLeaves);
  // CHECK: live=0 0

  return 0;
}
