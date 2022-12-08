// RUN: %target-swift-emit-silgen -module-name mod %s | %FileCheck %s

class Generic<T> {
  init() { preconditionFailure("death") }

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC7genericxvM : $@yield_once @convention(method) <T> (@guaranteed Generic<T>) -> @yields @inout T
  var generic: T

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC15genericFunctionxycvM : $@yield_once @convention(method) <T> (@guaranteed Generic<T>) -> @yields @inout @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>
  var genericFunction: () -> T

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC09returningB0xqd___tcluiM : $@yield_once @convention(method) <T><U> (@in_guaranteed U, @guaranteed Generic<T>) -> @yields @inout T
  subscript<U>(returningGeneric i: U) -> T {
    get { return generic }
    set {}
  }

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC012returningOwnB0qd__qd___tcluiM : $@yield_once @convention(method) <T><U> (@in_guaranteed U, @guaranteed Generic<T>) -> @yields @inout U {
  subscript<U>(returningOwnGeneric i: U) -> U {
    get { return i }
    set {}
  }

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC12complexTuplexSg_SDySSxGtvM : $@yield_once @convention(method) <T> (@guaranteed Generic<T>) -> @yields @inout (Optional<T>, Dictionary<String, T>)
  var complexTuple: (T?, [String: T])
}

class ConcreteWithInt: Generic<Int> {
  override init() { preconditionFailure("death") }

  // The concrete implementations.  Not actually important.
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC7genericSivM : $@yield_once @convention(method) (@guaranteed ConcreteWithInt) -> @yields @inout Int
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC15genericFunctionSiycvM : $@yield_once @convention(method) (@guaranteed ConcreteWithInt) -> @yields @inout @callee_guaranteed () -> Int
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC16returningGenericSix_tcluiM : $@yield_once @convention(method) <U> (@in_guaranteed U, @guaranteed ConcreteWithInt) -> @yields @inout Int
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC19returningOwnGenericxx_tcluiM : $@yield_once @convention(method) <U> (@in_guaranteed U, @guaranteed ConcreteWithInt) -> @yields @inout U
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC12complexTupleSiSg_SDySSSiGtvM : $@yield_once @convention(method) (@guaranteed ConcreteWithInt) -> @yields @inout (Optional<Int>, Dictionary<String, Int>)

  // The override thunks.  Note that the yields all exactly match the
  // original methods above in terms of where archetypes / type parameters
  // appear.

  // CHECK-LABEL: sil private [thunk] [ossa] @$s3mod15ConcreteWithIntC7genericSivMAA7GenericCADxvMTV : $@yield_once @convention(method) @substituted <τ_0_0> (@guaranteed ConcreteWithInt) -> @yields @inout τ_0_0 for <Int>
  override var generic: Int {
    get { super.generic }
    set {}
  }

  // CHECK-LABEL: sil private [thunk] [ossa] @$s3mod15ConcreteWithIntC15genericFunctionSiycvMAA7GenericCADxycvMTV : $@yield_once @convention(method) @substituted <τ_0_0> (@guaranteed ConcreteWithInt) -> (@yields @inout @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>) for <Int>
  override var genericFunction: () -> Int {
    get { super.genericFunction }
    set {}
  }

  // CHECK-LABEL: sil private [thunk] [ossa] @$s3mod15ConcreteWithIntC16returningGenericSix_tcluiMAA0F0CADxqd___tcluiMTV : $@yield_once @convention(method) <τ_0_0> @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0, @guaranteed ConcreteWithInt) -> @yields @inout τ_0_1 for <τ_0_0, Int>
  override subscript<U>(returningGeneric i: U) -> Int {
    get { return 0 }
    set {}
  }

  // This one doesn't need a thunk.
  override subscript<U>(returningOwnGeneric i: U) -> U {
    get { return  i }
    set {}
  }

  // CHECK-LABEL: sil private [thunk] [ossa] @$s3mod15ConcreteWithIntC12complexTupleSiSg_SDySSSiGtvMAA7GenericCADxSg_SDySSxGtvMTV : $@yield_once @convention(method) @substituted <τ_0_0, τ_0_1> (@guaranteed ConcreteWithInt) -> @yields @inout (Optional<τ_0_0>, Dictionary<String, τ_0_1>) for <Int, Int>
  override var complexTuple: (Int?, [String: Int]) {
    get { super.complexTuple }
    set {}
  }
}

protocol ProtoWithAssoc {
  associatedtype Assoc

  @_borrowed
  var generic: Assoc { get set }

  @_borrowed
  var genericFunction: () -> Assoc { get set }

  @_borrowed
  subscript<U>(returningGeneric i: U) -> Assoc { get set }

  @_borrowed
  subscript<U>(returningOwnGeneric i: U) -> U { get set }

  @_borrowed
  var complexTuple: (Assoc?, [String: Assoc]) { get set }
}
extension ConcreteWithInt : ProtoWithAssoc {
  // The unsubstituted yields here should match the natural
  // abstractions for the protocol.

  //   var generic
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP7generic0F0QzvrTW : $@yield_once @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_1 for <ConcreteWithInt, Int>
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP7generic0F0QzvMTW : $@yield_once @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1> (@inout τ_0_0) -> @yields @inout τ_0_1 for <ConcreteWithInt, Int>

  //   var genericFunction
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP15genericFunction0F0QzycvrTW : $@yield_once @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> (@yields @guaranteed @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_1>) for <ConcreteWithInt, Int>
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP15genericFunction0F0QzycvMTW : $@yield_once @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1> (@inout τ_0_0) -> (@yields @inout @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_1>) for <ConcreteWithInt, Int>

  //   subscript(returningGeneric:)
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP16returningGeneric0F0Qzqd___tcluirTW : $@yield_once @convention(witness_method: ProtoWithAssoc) <τ_0_0> @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @yields @in_guaranteed τ_0_2 for <τ_0_0, ConcreteWithInt, Int>
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP16returningGeneric0F0Qzqd___tcluiMTW : $@yield_once @convention(witness_method: ProtoWithAssoc) <τ_0_0> @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @inout τ_0_1) -> @yields @inout τ_0_2 for <τ_0_0, ConcreteWithInt, Int>

  //   subscript(returningOwnGeneric:)
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP19returningOwnGenericqd__qd___tcluirTW : $@yield_once @convention(witness_method: ProtoWithAssoc) <τ_0_0> @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @yields @in_guaranteed τ_0_2 for <τ_0_0, ConcreteWithInt, τ_0_0>
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP19returningOwnGenericqd__qd___tcluiMTW : $@yield_once @convention(witness_method: ProtoWithAssoc) <τ_0_0> @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @inout τ_0_1) -> @yields @inout τ_0_2 for <τ_0_0, ConcreteWithInt, τ_0_0>

  //   var complexTuple
  // CHECK-LABEL: sil shared [ossa] @$s3mod15ConcreteWithIntC12complexTupleSiSg_SDySSSiGtvr : $@yield_once @convention(method) (@guaranteed ConcreteWithInt) -> (@yields Optional<Int>, @yields @guaranteed Dictionary<String, Int>)
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP12complexTuple0F0QzSg_SDySSAHGtvMTW : $@yield_once @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1, τ_0_2> (@inout τ_0_0) -> @yields @inout (Optional<τ_0_1>, Dictionary<String, τ_0_2>) for <ConcreteWithInt, Int, Int>
}

// CHECK-LABEL: sil_vtable ConcreteWithInt {
// CHECK:         #Generic.generic!modify: <T> (Generic<T>) -> () -> () : @$s3mod15ConcreteWithIntC7genericSivMAA7GenericCADxvMTV [override]
// CHECK:         #Generic.genericFunction!modify: <T> (Generic<T>) -> () -> () : @$s3mod15ConcreteWithIntC15genericFunctionSiycvMAA7GenericCADxycvMTV [override]
// CHECK:         #Generic.subscript!modify: <T><U> (Generic<T>) -> (U) -> () : @$s3mod15ConcreteWithIntC16returningGenericSix_tcluiMAA0F0CADxqd___tcluiMTV [override]
// CHECK:         #Generic.subscript!modify: <T><U> (Generic<T>) -> (U) -> () : @$s3mod15ConcreteWithIntC19returningOwnGenericxx_tcluiM [override]
// CHECK:         #Generic.complexTuple!modify: <T> (Generic<T>) -> () -> () : @$s3mod15ConcreteWithIntC12complexTupleSiSg_SDySSSiGtvMAA7GenericCADxSg_SDySSxGtvMTV [override]
// CHECK:       }
