// RUN: %target-swift-emit-silgen -module-name mod %s | %FileCheck %s

class Generic<T> {
  init() { preconditionFailure("death") }

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC7genericxvx : $@yield_once_2 @convention(method) <T> (@guaranteed Generic<T>) -> @yields @inout T
  var generic: T

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC15genericFunctionxycvx : $@yield_once_2 @convention(method) <T> (@guaranteed Generic<T>) -> @yields @inout @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>
  var genericFunction: () -> T

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC09returningB0xqd___tcluix : $@yield_once_2 @convention(method) <T><U> (@in_guaranteed U, @guaranteed Generic<T>) -> @yields @inout T
  subscript<U>(returningGeneric i: U) -> T {
    get { return generic }
    set {}
  }

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC012returningOwnB0qd__qd___tcluix : $@yield_once_2 @convention(method) <T><U> (@in_guaranteed U, @guaranteed Generic<T>) -> @yields @inout U {
  subscript<U>(returningOwnGeneric i: U) -> U {
    get { return i }
    set {}
  }

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod7GenericC12complexTuplexSg_SDySSxGtvx : $@yield_once_2 @convention(method) <T> (@guaranteed Generic<T>) -> @yields @inout (Optional<T>, Dictionary<String, T>)
  var complexTuple: (T?, [String: T])
}

class ConcreteWithInt: Generic<Int> {
  override init() { preconditionFailure("death") }

  // The concrete implementations.  Not actually important.
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC7genericSivx : $@yield_once_2 @convention(method) (@guaranteed ConcreteWithInt) -> @yields @inout Int
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC15genericFunctionSiycvx : $@yield_once_2 @convention(method) (@guaranteed ConcreteWithInt) -> @yields @inout @callee_guaranteed () -> Int
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC16returningGenericSix_tcluix : $@yield_once_2 @convention(method) <U> (@in_guaranteed U, @guaranteed ConcreteWithInt) -> @yields @inout Int
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC19returningOwnGenericxx_tcluix : $@yield_once_2 @convention(method) <U> (@in_guaranteed U, @guaranteed ConcreteWithInt) -> @yields @inout U
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s3mod15ConcreteWithIntC12complexTupleSiSg_SDySSSiGtvx : $@yield_once_2 @convention(method) (@guaranteed ConcreteWithInt) -> @yields @inout (Optional<Int>, Dictionary<String, Int>)

  // The override thunks.  Note that the yields all exactly match the
  // original methods above in terms of where archetypes / type parameters
  // appear.

  // CHECK-LABEL: sil private [thunk] [ossa] @$s3mod15ConcreteWithIntC7genericSivxAA7GenericCADxvxTV : $@yield_once_2 @convention(method) @substituted <τ_0_0> (@guaranteed ConcreteWithInt) -> @yields @inout τ_0_0 for <Int>
  override var generic: Int {
    get { super.generic }
    set {}
  }

  // CHECK-LABEL: sil private [thunk] [ossa] @$s3mod15ConcreteWithIntC15genericFunctionSiycvxAA7GenericCADxycvxTV : $@yield_once_2 @convention(method) @substituted <τ_0_0> (@guaranteed ConcreteWithInt) -> (@yields @inout @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>) for <Int>
  override var genericFunction: () -> Int {
    get { super.genericFunction }
    set {}
  }

  // CHECK-LABEL: sil private [thunk] [ossa] @$s3mod15ConcreteWithIntC16returningGenericSix_tcluixAA0F0CADxqd___tcluixTV : $@yield_once_2 @convention(method) <τ_0_0> @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0, @guaranteed ConcreteWithInt) -> @yields @inout τ_0_1 for <τ_0_0, Int>
  override subscript<U>(returningGeneric i: U) -> Int {
    get { return 0 }
    set {}
  }

  // This one doesn't need a thunk.
  override subscript<U>(returningOwnGeneric i: U) -> U {
    get { return  i }
    set {}
  }

  // CHECK-LABEL: sil private [thunk] [ossa] @$s3mod15ConcreteWithIntC12complexTupleSiSg_SDySSSiGtvxAA7GenericCADxSg_SDySSxGtvxTV : $@yield_once_2 @convention(method) @substituted <τ_0_0, τ_0_1> (@guaranteed ConcreteWithInt) -> @yields @inout (Optional<τ_0_0>, Dictionary<String, τ_0_1>) for <Int, Int>
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
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP7generic0F0QzvyTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_1 for <ConcreteWithInt, Int>
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP7generic0F0QzvxTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1> (@inout τ_0_0) -> @yields @inout τ_0_1 for <ConcreteWithInt, Int>

  //   var genericFunction
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP15genericFunction0F0QzycvyTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> (@yields @guaranteed @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_1>) for <ConcreteWithInt, Int>
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP15genericFunction0F0QzycvxTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1> (@inout τ_0_0) -> (@yields @inout @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_1>) for <ConcreteWithInt, Int>

  //   subscript(returningGeneric:)
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP16returningGeneric0F0Qzqd___tcluiyTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) <τ_0_0> @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @yields @in_guaranteed τ_0_2 for <τ_0_0, ConcreteWithInt, Int>
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP16returningGeneric0F0Qzqd___tcluixTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) <τ_0_0> @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @inout τ_0_1) -> @yields @inout τ_0_2 for <τ_0_0, ConcreteWithInt, Int>

  //   subscript(returningOwnGeneric:)
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP19returningOwnGenericqd__qd___tcluiyTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) <τ_0_0> @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @yields @in_guaranteed τ_0_2 for <τ_0_0, ConcreteWithInt, τ_0_0>
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP19returningOwnGenericqd__qd___tcluixTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) <τ_0_0> @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @inout τ_0_1) -> @yields @inout τ_0_2 for <τ_0_0, ConcreteWithInt, τ_0_0>

  //   var complexTuple
  // CHECK-LABEL: sil shared [ossa] @$s3mod15ConcreteWithIntC12complexTupleSiSg_SDySSSiGtvy : $@yield_once_2 @convention(method) (@guaranteed ConcreteWithInt) -> (@yields Optional<Int>, @yields @guaranteed Dictionary<String, Int>)
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s3mod15ConcreteWithIntCAA05ProtoC5AssocA2aDP12complexTuple0F0QzSg_SDySSAHGtvxTW : $@yield_once_2 @convention(witness_method: ProtoWithAssoc) @substituted <τ_0_0, τ_0_1, τ_0_2> (@inout τ_0_0) -> @yields @inout (Optional<τ_0_1>, Dictionary<String, τ_0_2>) for <ConcreteWithInt, Int, Int>
}

// CHECK-LABEL: sil_vtable ConcreteWithInt {
// CHECK:         #Generic.generic!yielding_mutate: <T> (Generic<T>) -> () -> () : @$s3mod15ConcreteWithIntC7genericSivxAA7GenericCADxvxTV [override]
// CHECK:         #Generic.genericFunction!yielding_mutate: <T> (Generic<T>) -> () -> () : @$s3mod15ConcreteWithIntC15genericFunctionSiycvxAA7GenericCADxycvxTV [override]
// CHECK:         #Generic.subscript!yielding_mutate: <T><U> (Generic<T>) -> (U) -> () : @$s3mod15ConcreteWithIntC16returningGenericSix_tcluixAA0F0CADxqd___tcluixTV [override]
// CHECK:         #Generic.subscript!yielding_mutate: <T><U> (Generic<T>) -> (U) -> () : @$s3mod15ConcreteWithIntC19returningOwnGenericxx_tcluix [override]
// CHECK:         #Generic.complexTuple!yielding_mutate: <T> (Generic<T>) -> () -> () : @$s3mod15ConcreteWithIntC12complexTupleSiSg_SDySSSiGtvxAA7GenericCADxSg_SDySSxGtvxTV [override]
// CHECK:       }
