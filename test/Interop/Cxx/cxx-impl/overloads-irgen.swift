// Verifies that a `@cxx @implementation` function implementing one member of
// a C++ overload set is emitted under that member's mangled symbol, and that
// Swift-side calls target the same foreign entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import Overloads


// Same-arity overloads, told apart by parameter type

// int overloadedByType(int x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z16overloadedByTypei
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByType@@YAHH@Z"
@cxx @implementation
public func overloadedByType(_ x: Int32) -> Int32 { return x + 1 }

// double overloadedByType(double x);
// CHECK-SYSV-LABEL: define{{.*}} double @_Z16overloadedByTyped
// CHECK-WIN-LABEL: define{{.*}} double @"?overloadedByType@@YANN@Z"
@cxx @implementation
public func overloadedByType(_ x: Double) -> Double { return x * 2 }

// int overloadedByType(int *p);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z16overloadedByTypePi
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByType@@YAHPEAH@Z"
@cxx @implementation
public func overloadedByType(_ p: UnsafeMutablePointer<Int32>?) -> Int32 { return p!.pointee }


// Overloads told apart by arity and by parameter type

// int overloadedByArityAndType(int x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z24overloadedByArityAndTypei
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArityAndType@@YAHH@Z"
@cxx @implementation
public func overloadedByArityAndType(_ x: Int32) -> Int32 { return x + 1 }

// double overloadedByArityAndType(double x);
// CHECK-SYSV-LABEL: define{{.*}} double @_Z24overloadedByArityAndTyped
// CHECK-WIN-LABEL: define{{.*}} double @"?overloadedByArityAndType@@YANN@Z"
@cxx @implementation
public func overloadedByArityAndType(_ x: Double) -> Double { return x * 2 }

// int overloadedByArityAndType(int x, int y);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z24overloadedByArityAndTypeii
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArityAndType@@YAHHH@Z"
@cxx @implementation
public func overloadedByArityAndType(_ x: Int32, _ y: Int32) -> Int32 { return x + y }


// Both overloads implemented under Swift names, via `@cxx(...)`

// int renamedOverload(int x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z15renamedOverloadi
// CHECK-WIN-LABEL: define{{.*}} i32 @"?renamedOverload@@YAHH@Z"
@cxx(renamedOverload) @implementation
public func swiftRenamedOverloadInt(_ x: Int32) -> Int32 { return x + 1 }

// double renamedOverload(double x);
// CHECK-SYSV-LABEL: define{{.*}} double @_Z15renamedOverloadd
// CHECK-WIN-LABEL: define{{.*}} double @"?renamedOverload@@YANN@Z"
@cxx(renamedOverload) @implementation
public func swiftRenamedOverloadDouble(_ x: Double) -> Double { return x * 2 }


// Swift-side calls

// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}19callOverloadedFuncsyyF"
// CHECK-SYSV:   invoke i32 @_Z16overloadedByTypei
// CHECK-SYSV:   invoke double @_Z16overloadedByTyped
// CHECK-SYSV:   invoke i32 @_Z16overloadedByTypePi
// CHECK-SYSV:   invoke i32 @_Z24overloadedByArityAndTypei
// CHECK-SYSV:   invoke double @_Z24overloadedByArityAndTyped
// CHECK-SYSV:   invoke i32 @_Z24overloadedByArityAndTypeii
// CHECK-SYSV:   invoke i32 @_Z15renamedOverloadi
// CHECK-SYSV:   invoke double @_Z15renamedOverloadd
public func callOverloadedFuncs() {
  var x: Int32 = 42
  _ = overloadedByType(x)
  _ = overloadedByType(1.5)
  _ = overloadedByType(&x)

  _ = overloadedByArityAndType(x)
  _ = overloadedByArityAndType(1.5)
  _ = overloadedByArityAndType(x, x)

  _ = renamedOverload(x)
  _ = renamedOverload(1.5)
}
