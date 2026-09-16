// Verifies that a `@cxx @implementation` function implementing one member of
// a C++ overload set is emitted under that member's mangled symbol, and that
// Swift-side calls target the same foreign entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi,CHECK-%target-abi-%target-ptrsize

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

// int overloadedByType(Point p);
// The 8-byte struct is coerced into one `i64` by the C++ ABI.
// CHECK-SYSV-64-LABEL: define{{.*}} i32 @_Z16overloadedByType5Point(i64
// CHECK-SYSV-32-LABEL: define{{.*}} i32 @_Z16overloadedByType5Point([2 x i32]
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByType@@YAHUPoint@@@Z"
@cxx @implementation
public func overloadedByType(_ p: Point) -> Int32 { return p.x + p.y }


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

// int overloadedByArityAndType(Point p, int z);
// CHECK-SYSV-64-LABEL: define{{.*}} i32 @_Z24overloadedByArityAndType5Pointi(i64
// CHECK-SYSV-32-LABEL: define{{.*}} i32 @_Z24overloadedByArityAndType5Pointi([2 x i32]
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArityAndType@@YAHUPoint@@H@Z"
@cxx @implementation
public func overloadedByArityAndType(_ p: Point, _ z: Int32) -> Int32 { return (p.x + p.y) * z }


// Overloads on enum types

// int overloadedByEnumType(int x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z20overloadedByEnumTypei
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByEnumType@@YAHH@Z"
@cxx @implementation
public func overloadedByEnumType(_ x: Int32) -> Int32 { return x }

// int overloadedByEnumType(unsigned x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z20overloadedByEnumTypej
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByEnumType@@YAHI@Z"
@cxx @implementation
public func overloadedByEnumType(_ x: UInt32) -> Int32 { return Int32(x) + 10 }

// int overloadedByEnumType(EnumFoo x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z20overloadedByEnumType7EnumFoo(i32
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByEnumType@@YAHW4EnumFoo@@@Z"
@cxx @implementation
public func overloadedByEnumType(_ x: EnumFoo) -> Int32 { return Int32(x.rawValue) + 20 }

// int overloadedByEnumType(EnumBar x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z20overloadedByEnumType7EnumBar(i32
// CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByEnumType@@YAHW4EnumBar@@@Z"
@cxx @implementation
public func overloadedByEnumType(_ x: EnumBar) -> Int32 { return x.rawValue + 30 }


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
// CHECK-SYSV:   invoke i32 @_Z16overloadedByType5Point
// CHECK-SYSV:   invoke i32 @_Z24overloadedByArityAndTypei
// CHECK-SYSV:   invoke double @_Z24overloadedByArityAndTyped
// CHECK-SYSV:   invoke i32 @_Z24overloadedByArityAndTypeii
// CHECK-SYSV:   invoke i32 @_Z24overloadedByArityAndType5Pointi
// CHECK-SYSV:   invoke i32 @_Z20overloadedByEnumTypei
// CHECK-SYSV:   invoke i32 @_Z20overloadedByEnumTypej
// CHECK-SYSV:   invoke i32 @_Z20overloadedByEnumType7EnumFoo
// CHECK-SYSV:   invoke i32 @_Z20overloadedByEnumType7EnumBar
// CHECK-SYSV:   invoke i32 @_Z15renamedOverloadi
// CHECK-SYSV:   invoke double @_Z15renamedOverloadd
public func callOverloadedFuncs() {
  var x: Int32 = 42
  _ = overloadedByType(x)
  _ = overloadedByType(1.5)
  _ = overloadedByType(&x)
  _ = overloadedByType(Point(x: 1, y: 2))

  _ = overloadedByArityAndType(x)
  _ = overloadedByArityAndType(1.5)
  _ = overloadedByArityAndType(x, x)
  _ = overloadedByArityAndType(Point(x: 1, y: 2), x)

  let u: UInt32 = 42
  _ = overloadedByEnumType(x)
  _ = overloadedByEnumType(u)
  _ = overloadedByEnumType(FooA)
  _ = overloadedByEnumType(EnumBar.BarA)

  _ = renamedOverload(x)
  _ = renamedOverload(1.5)
}
