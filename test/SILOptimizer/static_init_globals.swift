// RUN: %target-swift-frontend %s -module-name=test -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend %s -O -module-name=test -emit-sil | %FileCheck %s --check-prefix=CHECK --check-prefix=OPT

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

public struct GenS<T> {
  var a: T
  var b: T
  var c: T
  var d: T
  var e: T
  var f: T
  var g: T
  var h: T
}

public struct MyStruct {
  // CHECK-LABEL: sil_global [let] @$s4test8MyStructV1rSnySiGvpZ : $Range<Int> = {
  // CHECK-NEXT:    %0 = integer_literal $Builtin.Int{{[0-9]+}}, 1
  // CHECK-NEXT:    %1 = struct $Int (%0)
  // CHECK-NEXT:    %2 = integer_literal $Builtin.Int{{[0-9]+}}, 3
  // CHECK-NEXT:    %3 = struct $Int (%2)
  // CHECK-NEXT:    %initval = struct $Range<Int> (%1, %3)
  // CHECK-NEXT:  }
  public static let r: Range<Int> = 1 ..< 3

  // OPT-LABEL: sil_global [let] @$s4test8MyStructV07genericC0AA4GenSVySiGvpZ : $GenS<Int> = {
  // OPT:         %initval = struct $GenS
  // OPT-NEXT:  }
  public static let genericStruct = GenS(a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8)
}

