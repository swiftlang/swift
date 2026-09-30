// Verifies that a `@cxx @implementation` function in an extension of an
// imported C++ namespace is emitted under the namespace-qualified mangled
// symbol of the C++ declaration it implements and that Swift-side calls target
// the same foreign entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import Namespaces


extension Outer {
  // int Outer::add(int a, int b);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN5Outer3addEii(i32 %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?add@Outer@@YAHHH@Z"(i32 %0, i32 %1)
  @cxx @implementation
  public static func add(_ a: Int32, _ b: Int32) -> Int32 { return a + b }

  // int Outer::renamedTarget(int x);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN5Outer13renamedTargetEi
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?renamedTarget@Outer@@YAHH@Z"
  @cxx(renamedTarget) @implementation
  public static func swiftRenamed(_ x: Int32) -> Int32 { return x * 2 }

  // int Outer::overloadedByArity(int x);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN5Outer17overloadedByArityEi
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArity@Outer@@YAHH@Z"
  @cxx @implementation
  public static func overloadedByArity(_ x: Int32) -> Int32 { return x + 1 }

  // int Outer::overloadedByArity(int x, int y);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN5Outer17overloadedByArityEii
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArity@Outer@@YAHHH@Z"
  @cxx @implementation
  public static func overloadedByArity(_ x: Int32, _ y: Int32) -> Int32 { return x + y }
}

extension Outer.Inner {
  // int Outer::Inner::nestedFunc(int x);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN5Outer5Inner10nestedFuncEi
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?nestedFunc@Inner@Outer@@YAHH@Z"
  @cxx @implementation
  public static func nestedFunc(_ x: Int32) -> Int32 { return x }
}


// Swift-side calls

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}18callNamespaceFuncss5Int32VyF"
// CHECK-SYSV:   invoke i32 @_ZN5Outer3addEii
// CHECK-SYSV:   invoke i32 @_ZN5Outer13renamedTargetEi
// CHECK-SYSV:   invoke i32 @_ZN5Outer17overloadedByArityEi
// CHECK-SYSV:   invoke i32 @_ZN5Outer17overloadedByArityEii
// CHECK-SYSV:   invoke i32 @_ZN5Outer5Inner10nestedFuncEi
public func callNamespaceFuncs() -> Int32 {
  return Outer.add(1, 2) + Outer.renamedTarget(4) + Outer.overloadedByArity(5)
    + Outer.overloadedByArity(6, 7) + Outer.Inner.nestedFunc(3)
}
