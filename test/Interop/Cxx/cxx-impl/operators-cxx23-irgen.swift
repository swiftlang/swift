// Verifies that a `@cxx @implementation` of a C++23 operator is emitted under
// its mangled symbol, and that Swift-side uses call it.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -Xcc -std=c++23 \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import OperatorsCxx23


extension Grid {
  // int Grid::operator[]() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4GridixEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"??AGrid@@QEBAHXZ"(ptr %0)
  @cxx(`operator[]`) @implementation
  public func first() -> Int32 { return width }

  // int Grid::operator[](int i) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4GridixEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"??AGrid@@QEBAHH@Z"(ptr %0, i32 %1)
  @cxx(`operator[]`) @implementation
  public func at(_ i: Int32) -> Int32 { return i }

  // int Grid::operator[](int row, int col) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4GridixEii(ptr %0, i32 %1, i32 %2)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"??AGrid@@QEBAHHH@Z"(ptr %0, i32 %1, i32 %2)
  @cxx(`operator[]`) @implementation
  public func at(_ row: Int32, _ col: Int32) -> Int32 { return row * width + col }

  // double Grid::operator[](double row, double col) const;
  // CHECK-SYSV-LABEL: define{{.*}} double @_ZNK4GridixEdd(ptr %0, double %1, double %2)
  // CHECK-WIN-LABEL: define{{.*}} double @"??AGrid@@QEBANNN@Z"(ptr %0, double %1, double %2)
  @cxx(`operator[]`) @implementation
  public func at(_ row: Double, _ col: Double) -> Double {
    return row * Double(width) + col
  }

  // int &Grid::operator[](int row, int col, int layer);
  // CHECK-SYSV-LABEL: define{{.*}} ptr @_ZN4GridixEiii(ptr %0, i32 %1, i32 %2, i32 %3)
  // CHECK-WIN-LABEL: define{{.*}} ptr @"??AGrid@@QEAAAEAHHHH@Z"(ptr %0, i32 %1, i32 %2, i32 %3)
  @cxx(`operator[]`) @implementation
  public mutating func at(_ row: Int32, _ col: Int32, _ layer: Int32) -> UnsafeMutablePointer<Int32> {
    return withUnsafeMutablePointer(to: &width) { $0 }
  }
}

extension StaticGrid {
  // static int StaticGrid::operator[](int i);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN10StaticGridixEi(i32 %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"??AStaticGrid@@SAHH@Z"(i32 %0)
  @cxx(`operator[]`) @implementation
  public static func at(_ i: Int32) -> Int32 { return i * 2 }

  // static int StaticGrid::operator[](int row, int col);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN10StaticGridixEii(i32 %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"??AStaticGrid@@SAHHH@Z"(i32 %0, i32 %1)
  @cxx(`operator[]`) @implementation
  public static func at(_ row: Int32, _ col: Int32) -> Int32 { return row * 10 + col }
}

// Swift-side uses call the same entry points.
// CHECK-LABEL: define{{.*}} @"$s{{.*}}14callSubscripts
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK4GridixEv(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK4GridixEi(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK4GridixEii(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK4GridixEdd(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZN4GridixEiii(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZN10StaticGridixEi(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZN10StaticGridixEii(
public func callSubscripts(_ g: inout Grid, _ s: StaticGrid) -> Int32 {
  g[0, 0, 0] = 5
  return g[] + g[1] + g[1, 2] + Int32(g[1.5, 2.5]) + s[3] + s[1, 2]
}
