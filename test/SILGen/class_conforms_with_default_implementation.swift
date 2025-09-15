// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol P {
  associatedtype T : Q
  func foo()
}

extension P {
  func foo() {}
}

protocol Q {}

class C<T : Q> : P {}

protocol PP {
  associatedtype T : QQ
  func foo()
}

extension PP {
  func foo() {}
}

class QQ {}

class CC<T : QQ> : PP {}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s42class_conforms_with_default_implementation1CCyqd__GAA1PA2aEP3fooyyFTW : $@convention(witness_method: P) <τ_0_0><τ_1_0 where τ_0_0 : C<τ_1_0>, τ_1_0 : Q> (@in_guaranteed τ_0_0) -> () {
// CHECK: [[WITNESS:%.*]] = function_ref @$s42class_conforms_with_default_implementation1PPAAE3fooyyF : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply [[WITNESS]]<τ_0_0>(%0) : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()
// CHECK: return

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s42class_conforms_with_default_implementation2CCCyqd__GAA2PPA2aEP3fooyyFTW : $@convention(witness_method: PP) <τ_0_0><τ_1_0 where τ_0_0 : CC<τ_1_0>, τ_1_0 : QQ> (@in_guaranteed τ_0_0) -> () {
// CHECK: [[WITNESS:%.*]] = function_ref @$s42class_conforms_with_default_implementation2PPPAAE3fooyyF : $@convention(method) <τ_0_0 where τ_0_0 : PP> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply [[WITNESS]]<τ_0_0>(%0) : $@convention(method) <τ_0_0 where τ_0_0 : PP> (@in_guaranteed τ_0_0) -> ()
// CHECK: return

// CHECK-LABEL: sil_witness_table hidden <T where T : Q> C<T>: P module class_conforms_with_default_implementation {
// CHECK-NEXT: associated_conformance (T: Q): dependent
// CHECK-NEXT: associated_type T: T
// CHECK-NEXT: method #P.foo: <Self where Self : P> (Self) -> () -> () : @$s42class_conforms_with_default_implementation1CCyqd__GAA1PA2aEP3fooyyFTW
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden <T where T : QQ> CC<T>: PP module class_conforms_with_default_implementation {
// CHECK-NEXT: associated_type T: T
// CHECK-NEXT: method #PP.foo: <Self where Self : PP> (Self) -> () -> () : @$s42class_conforms_with_default_implementation2CCCyqd__GAA2PPA2aEP3fooyyFTW
// CHECK-NEXT: }
