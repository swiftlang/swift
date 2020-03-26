// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -disable-availability-checking -primary-file %s %S/Inputs/specialize_opaque_type_archetypes_multifile_A.swift -emit-sil | %FileCheck %s

protocol P {}

extension Int : P {}

struct Pair {
  var x: Int64 = 0
  var y: Int64 = 0
}

extension Pair : P {}

@inline(never)
func bar(_ x: Int) -> some P {
  return x
}

// CHECK-LABEL: sil @$s43specialize_opaque_type_archetypes_multifile4testyyF : $@convention(thin) () -> () {
// CHECK: function_ref @$s43specialize_opaque_type_archetypes_multifile4bar2yQrSiF : $@convention(thin) (Int) -> @out @_opaqueReturnTypeOf("$s43specialize_opaque_type_archetypes_multifile4bar2yQrSiF", 0)
public func test() {
  print(bar(5))
  print(bar2(5))
}
