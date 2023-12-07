// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/opaque_result_type_private_assoc_type_other.swift -emit-module-path %t/opaque_result_type_private_assoc_type_other.swiftmodule -disable-availability-checking
// RUN: %target-swift-emit-silgen -I %t %s | %FileCheck %s

import opaque_result_type_private_assoc_type_other

// CHECK-LABEL: sil hidden [ossa] @$s033opaque_result_type_private_assoc_C0028usesAssocTypeOfPrivateResultH0yyF : $@convention(thin) () -> () {
func usesAssocTypeOfPrivateResultType() {
  // CHECK: [[BOX:%.*]] = alloc_stack $Optional<(@_opaqueReturnTypeOf("$s033opaque_result_type_private_assoc_C6_other11doSomethingQryF", 0) __).Element>
  // CHECK: [[METHOD:%.*]] = witness_method $@_opaqueReturnTypeOf("$s033opaque_result_type_private_assoc_C6_other11doSomethingQryF", 0) __, #IteratorProtocol.next : <Self where Self : opaque_result_type_private_assoc_type_other.IteratorProtocol> (Self) -> () -> Self.Element? : $@convention(witness_method: IteratorProtocol) <τ_0_0 where τ_0_0 : IteratorProtocol> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0.Element>
  // CHECK:  [[RESULT:%.*]] = apply [[METHOD]]<@_opaqueReturnTypeOf("$s033opaque_result_type_private_assoc_C6_other11doSomethingQryF", 0) __>([[BOX]], {{%.*}}) : $@convention(witness_method: IteratorProtocol) <τ_0_0 where τ_0_0 : IteratorProtocol> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0.Element>
  let iterator = doSomething()
  let _ = iterator.next()
}
