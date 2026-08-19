// Verifies that a `@cxx @implementation` function taking or returning a
// foreign reference type is emitted under the mangled symbol of the C++
// function it implements, with the reference lowered to a plain pointer, and
// that Swift-side calls target the same foreign entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-availability-checking \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import ForeignReference


// int takesNode(Node *_Nonnull n);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z9takesNodeP4Node(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?takesNode@@YAHPEAUNode@@@Z"(ptr %0)
@cxx @implementation
public func takesNode(_ n: Node) -> Int32 { return n.value }

// int takesNullableNode(Node *_Nullable n);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z17takesNullableNodeP4Node(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?takesNullableNode@@YAHPEAUNode@@@Z"(ptr %0)
@cxx @implementation
public func takesNullableNode(_ n: Node?) -> Int32 { return n?.value ?? -1 }

// Node *_Nonnull returnsRetainedNode(Node *_Nonnull n)
//     __attribute__((swift_attr("returns_retained")));
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z19returnsRetainedNodeP4Node(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} ptr @"?returnsRetainedNode@@YAPEAUNode@@PEAU1@@Z"(ptr %0)
@cxx @implementation
public func returnsRetainedNode(_ n: Node) -> Node { return n }

// Node *_Nullable returnsNullableRetainedNode(Node *_Nonnull n, int null)
//     __attribute__((swift_attr("returns_retained")));
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z27returnsNullableRetainedNodeP4Nodei(ptr %0, i32 %1)
// CHECK-WIN-LABEL: define{{.*}} ptr @"?returnsNullableRetainedNode@@YAPEAUNode@@PEAU1@H@Z"(ptr %0, i32 %1)
@cxx @implementation
public func returnsNullableRetainedNode(_ n: Node, _ null: Int32) -> Node? {
  return null != 0 ? nil : n
}

extension Node {
  // static Node *_Nonnull Node::passThrough(Node *_Nonnull n)
  //     __attribute__((swift_attr("returns_retained")));
  // CHECK-SYSV-LABEL: define{{.*}} ptr @_ZN4Node11passThroughEPS_(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} ptr @"?passThrough@Node@@SAPEAU1@PEAU1@@Z"(ptr %0)
  @cxx @implementation
  public static func passThrough(_ n: Node) -> Node { return n }
}

// Leaf *_Nonnull returnsRetainedLeaf(Leaf *_Nonnull l)
//     __attribute__((swift_attr("returns_retained")));
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z19returnsRetainedLeafP4Leaf(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} ptr @"?returnsRetainedLeaf@@YAPEAULeaf@@PEAU1@@Z"(ptr %0)
@cxx @implementation
public func returnsRetainedLeaf(_ l: Leaf) -> Leaf { return l }

// Singleton *_Nonnull returnsSingleton(Singleton *_Nonnull s);
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z16returnsSingletonP9Singleton(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} ptr @"?returnsSingleton@@YAPEAUSingleton@@PEAU1@@Z"(ptr %0)
@cxx @implementation
public func returnsSingleton(_ s: Singleton) -> Singleton { return s }


// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}12callCxxFuncsyySo4NodeVF"(ptr %0)
// CHECK-SYSV:   invoke i32 @_Z9takesNodeP4Node(ptr %0)
// CHECK-SYSV:   invoke i32 @_Z17takesNullableNodeP4Node(ptr null)
// CHECK-SYSV:   invoke ptr @_Z19returnsRetainedNodeP4Node(ptr %0)
// CHECK-SYSV:   invoke ptr @_Z27returnsNullableRetainedNodeP4Nodei(ptr %0, i32 1)
// CHECK-SYSV:   invoke ptr @_ZN4Node11passThroughEPS_(ptr %0)
public func callCxxFuncs(_ n: Node) {
  _ = takesNode(n)
  _ = takesNullableNode(nil)
  _ = returnsRetainedNode(n)
  _ = returnsNullableRetainedNode(n, 1)
  _ = Node.passThrough(n)
}
