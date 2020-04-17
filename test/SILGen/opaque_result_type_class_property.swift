// RUN: %target-swift-emit-silgen %s -enable-library-evolution -disable-availability-checking | %FileCheck %s

public protocol V {}
public struct E : V {}

public class HasProperty {
  public var foo: some V = E()
}

// CHECK-LABEL: sil shared [thunk] [ossa] @$s33opaque_result_type_class_property11HasPropertyC3fooQrvpACTK : $@convention(thin) (@in_guaranteed HasProperty) -> @out @_opaqueReturnTypeOf("$s33opaque_result_type_class_property11HasPropertyC3fooQrvp", 0) 🦸 {
// CHECK: bb0(%0 : $*E, %1 : $*HasProperty):

// CHECK-LABEL: sil shared [thunk] [ossa] @$s33opaque_result_type_class_property11HasPropertyC3fooQrvpACTk : $@convention(thin) (@in_guaranteed @_opaqueReturnTypeOf("$s33opaque_result_type_class_property11HasPropertyC3fooQrvp", 0) 🦸, @in_guaranteed HasProperty) -> () {
// CHECK: bb0(%0 : $*E, %1 : $*HasProperty):
