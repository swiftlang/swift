// RUN: %target-swift-frontend %s -module-name=test -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

public struct MyStruct {
  // CHECK-LABEL: sil_global [let] @$s4test8MyStructV1rSnySiGvpZ : $Range<Int> = {
  // CHECK-NEXT:    %0 = integer_literal $Builtin.Int{{[0-9]+}}, 1
  // CHECK-NEXT:    %1 = struct $Int (%0)
  // CHECK-NEXT:    %2 = integer_literal $Builtin.Int{{[0-9]+}}, 3
  // CHECK-NEXT:    %3 = struct $Int (%2)
  // CHECK-NEXT:    %initval = struct $Range<Int> (%1, %3)
  // CHECK-NEXT:  }
  public static let r: Range<Int> = 1 ..< 3
}

