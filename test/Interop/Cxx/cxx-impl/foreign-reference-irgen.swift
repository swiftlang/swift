// Verifies that a `@cxx @implementation` function taking or returning a
// foreign reference type, or a method of one, is emitted under the mangled
// symbol of the C++ function it implements, with the reference lowered to a
// plain pointer, and that Swift-side calls target the same foreign entry
// points.

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


// The `self` of an instance method is `this`.

extension Node {
  // int Node::get() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4Node3getEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?get@Node@@QEBAHXZ"(ptr %0)
  // CHECK: getelementptr inbounds{{.*}} %TSo4NodeV, ptr %0
  @cxx @implementation
  public func get() -> Int32 { return value }

  // void Node::add(int d);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN4Node3addEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} void @"?add@Node@@QEAAXH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public func add(_ d: Int32) { value += d }

  // int Node::overloadedByType(int x) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4Node16overloadedByTypeEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByType@Node@@QEBAHH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public func overloadedByType(_ x: Int32) -> Int32 { return value + x }

  // double Node::overloadedByType(double x) const;
  // CHECK-SYSV-LABEL: define{{.*}} double @_ZNK4Node16overloadedByTypeEd(ptr %0, double %1)
  // CHECK-WIN-LABEL: define{{.*}} double @"?overloadedByType@Node@@QEBANN@Z"(ptr %0, double %1)
  @cxx @implementation
  public func overloadedByType(_ x: Double) -> Double { return Double(value) + x }
}


// Methods of an immortal foreign reference type: `self` is `this`, and neither
// it nor a returned `this` is retained or released.

extension Singleton {
  // int Singleton::read() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK9Singleton4readEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?read@Singleton@@QEBAHXZ"(ptr %0)
  // CHECK: getelementptr inbounds{{.*}} %TSo9SingletonV, ptr %0
  @cxx @implementation
  public func read() -> Int32 { return value }

  // Singleton *_Nonnull Singleton::itself() const;
  // CHECK-SYSV-LABEL: define{{.*}} ptr @_ZNK9Singleton6itselfEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} ptr @"?itself@Singleton@@QEBAPEAU1@XZ"(ptr %0)
  // CHECK-NOT: {{retain|release}}
  // CHECK: ret ptr %0
  @cxx @implementation
  public func itself() -> Singleton { return self }
}


// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}12callCxxFuncsyySo4NodeVF"(ptr %0)
// CHECK-SYSV:   invoke i32 @_Z9takesNodeP4Node(ptr %0)
// CHECK-SYSV:   invoke i32 @_Z17takesNullableNodeP4Node(ptr null)
// CHECK-SYSV:   invoke ptr @_Z19returnsRetainedNodeP4Node(ptr %0)
// CHECK-SYSV:   invoke ptr @_Z27returnsNullableRetainedNodeP4Nodei(ptr %0, i32 1)
// CHECK-SYSV:   invoke i32 @_ZNK4Node3getEv(ptr %0)
// CHECK-SYSV:   invoke void @_ZN4Node3addEi(ptr %0, i32 2)
// CHECK-SYSV:   invoke i32 @_ZNK4Node16overloadedByTypeEi(ptr %0, i32 3)
// CHECK-SYSV:   invoke double @_ZNK4Node16overloadedByTypeEd(ptr %0, double 1.500000e+00)
public func callCxxFuncs(_ n: Node) {
  _ = takesNode(n)
  _ = takesNullableNode(nil)
  _ = returnsRetainedNode(n)
  _ = returnsNullableRetainedNode(n, 1)
  _ = n.get()
  n.add(2)
  let x: Int32 = 3
  _ = n.overloadedByType(x)
  _ = n.overloadedByType(1.5)
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}19callImmortalMethodsyySo9SingletonVF"(ptr %0)
// CHECK-SYSV:   invoke i32 @_ZNK9Singleton4readEv(ptr %0)
// CHECK-SYSV:   invoke ptr @_ZNK9Singleton6itselfEv(ptr %0)
public func callImmortalMethods(_ s: Singleton) {
  _ = s.read()
  _ = s.itself()
}
