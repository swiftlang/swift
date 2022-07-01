// RUN: %target-swift-frontend  -primary-file %s -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s

// Make a runtime test to check that the values are correct.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib


struct S {
  var i: Int
  var b: Int8

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV4sizeSivpZ : $Int = {
  // CHECK-NEXT:    integer_literal
  static let size = MemoryLayout<S>.size

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV9alignmentSivpZ : $Int = {
  // CHECK-NEXT:    integer_literal
  static let alignment = MemoryLayout<S>.alignment

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV6strideSivpZ : $Int = {
  // CHECK-NEXT:    integer_literal
  static let stride = MemoryLayout<S>.stride

  static let doubleSize = MemoryLayout<S>.size * 2
}

// CHECK-LABEL: sil @$s4test14noConstantSizeySixmlF
// CHECK:         builtin "sizeof"<T>
// CHECK:       } // end sil function '$s4test14noConstantSizeySixmlF'
public func noConstantSize<T>(_ t: T.Type) -> Int {
  return MemoryLayout<T>.size
}

// Check that there is not constant propagation if optimizations are disabled.
// This is important for the runtime check to make sure that we are comparing
// SIL constant propagated values with IRGen values.

// CHECK-LABEL: sil {{.*}} @$s4test7getSizeSiyF
// CHECK:         builtin "sizeof"<S>
// CHECK:       } // end sil function '$s4test7getSizeSiyF' 
@_optimize(none)
func getSize() -> Int {
  return MemoryLayout<S>.size
}

// CHECK-LABEL: sil {{.*}} @$s4test12getAlignmentSiyF
// CHECK:         builtin "alignof"<S>
// CHECK:       } // end sil function '$s4test12getAlignmentSiyF' 
@_optimize(none)
func getAlignment() -> Int {
  return MemoryLayout<S>.alignment
}

// CHECK-LABEL: sil {{.*}} @$s4test9getStrideSiyF
// CHECK:         builtin "strideof"<S>
// CHECK:       } // end sil function '$s4test9getStrideSiyF' 
@_optimize(none)
func getStride() -> Int {
  return MemoryLayout<S>.stride
}

@inline(never)
func testit() {
  // CHECK-OUTPUT: size: true
  print("size: \(S.size == getSize())")

  // CHECK-OUTPUT: alignment: true
  print("alignment: \(S.alignment == getAlignment())")

  // CHECK-OUTPUT: stride: true
  print("stride: \(S.stride == getStride())")

  // CHECK-OUTPUT: doubleSize: true
  print("doubleSize: \(S.doubleSize == getSize() * 2)")
}

testit()

