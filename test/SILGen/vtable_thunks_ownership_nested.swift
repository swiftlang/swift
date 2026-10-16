// RUN: %target-swift-emit-silgen -module-name vtable_thunks_ownership_nested %s | %FileCheck %s --enable-var-scope

// An override may change whether a closure in its signature takes its
// parameter owned. That changes the closure's lowered type, so the override
// gets its own vtable entry, and the entry it overrides gets a thunk that
// reabstracts the closure.

class V {}

class Base {
  func closureParam(_ g: (borrowing V) -> ()) {}
  func closureResult() -> (consuming V) -> () { { _ in } }
  var closureProperty: (borrowing V) -> () { { _ in } }
  func optionalClosureParam(_ g: @escaping (borrowing V) -> ()) {}
  func closureInTuple() -> ((borrowing V) -> (), Int) { ({ _ in }, 0) }
  func closureParamOfClosure(_ g: ((consuming V) -> ()) -> ()) {}
  func generic<T>(_ g: (borrowing T) -> ()) {}
  func defaultToBorrow(_ g: (V) -> ()) {}
  func consumeToSending(_ g: (consuming V) -> ()) {}
}

class Derived: Base {
  override func closureParam(_ g: (consuming V) -> ()) {}
  override func closureResult() -> (borrowing V) -> () { { _ in } }
  override var closureProperty: (consuming V) -> () { { _ in } }
  override func optionalClosureParam(_ g: ((consuming V) -> ())?) {}
  override func closureInTuple() -> ((consuming V) -> (), Int) { ({ _ in }, 0) }
  override func closureParamOfClosure(_ g: ((borrowing V) -> ()) -> ()) {}
  override func generic<T>(_ g: (consuming T) -> ()) {}
  override func defaultToBorrow(_ g: (borrowing V) -> ()) {}
  override func consumeToSending(_ g: (sending V) -> ()) {}
}

class MoreDerived: Derived {
  override func closureParam(_ g: (consuming V) -> ()) {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s30vtable_thunks_ownership_nested7DerivedC12closureParamyyyAA1VCnXEFAA4BaseCADyyyAFXEFTV : $@convention(method) (@guaranteed @noescape @callee_guaranteed (@guaranteed V) -> (), @guaranteed Derived) -> () {
// CHECK:       bb0([[G:%.*]] : @guaranteed $@noescape @callee_guaranteed (@guaranteed V) -> (), [[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[G_COPY:%.*]] = copy_value [[G]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$s30vtable_thunks_ownership_nested1VCIgg_ACIegx_TR : $@convention(thin) (@owned V, @guaranteed @noescape @callee_guaranteed (@guaranteed V) -> ()) -> ()
// CHECK:         [[PA:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[G_COPY]])
// CHECK:         [[NOESCAPE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PA]]
// CHECK:         [[BORROW:%.*]] = begin_borrow [[NOESCAPE]]
// CHECK:         [[FN:%.*]] = function_ref @$s30vtable_thunks_ownership_nested7DerivedC12closureParamyyyAA1VCnXEF : $@convention(method) (@guaranteed @noescape @callee_guaranteed (@owned V) -> (), @guaranteed Derived) -> ()
// CHECK:         apply [[FN]]([[BORROW]], [[SELF]])
// CHECK:       } // end sil function

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s30vtable_thunks_ownership_nested1VCIgg_ACIegx_TR : $@convention(thin) (@owned V, @guaranteed @noescape @callee_guaranteed (@guaranteed V) -> ()) -> () {
// CHECK:       bb0([[X:%.*]] : @owned $V, [[G:%.*]] : @guaranteed $@noescape @callee_guaranteed (@guaranteed V) -> ()):
// CHECK:         [[X_BORROW:%.*]] = begin_borrow [[X]]
// CHECK:         apply [[G]]([[X_BORROW]])
// CHECK:         end_borrow [[X_BORROW]]
// CHECK:         destroy_value [[X]]
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s30vtable_thunks_ownership_nested7DerivedC13closureResultyAA1VCcyFAA4BaseCADyAFncyFTV : $@convention(method) (@guaranteed Derived) -> @owned @callee_guaranteed (@owned V) -> () {
// CHECK:         [[FN:%.*]] = function_ref @$s30vtable_thunks_ownership_nested7DerivedC13closureResultyAA1VCcyF : $@convention(method) (@guaranteed Derived) -> @owned @callee_guaranteed (@guaranteed V) -> ()
// CHECK:         [[RESULT:%.*]] = apply [[FN]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$s30vtable_thunks_ownership_nested1VCIegg_ACIegx_TR : $@convention(thin) (@owned V, @guaranteed @callee_guaranteed (@guaranteed V) -> ()) -> ()
// CHECK:         [[PA:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[RESULT]])
// CHECK:         return [[PA]]
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s30vtable_thunks_ownership_nested7DerivedC15closurePropertyyyAA1VCncvgAA4BaseCADyyAFcvgTV : $@convention(method) (@guaranteed Derived) -> @owned @callee_guaranteed (@guaranteed V) -> () {
// CHECK:         [[FN:%.*]] = function_ref @$s30vtable_thunks_ownership_nested7DerivedC15closurePropertyyyAA1VCncvg : $@convention(method) (@guaranteed Derived) -> @owned @callee_guaranteed (@owned V) -> ()
// CHECK:         [[RESULT:%.*]] = apply [[FN]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$s30vtable_thunks_ownership_nested1VCIegx_ACIegg_TR : $@convention(thin) (@guaranteed V, @guaranteed @callee_guaranteed (@owned V) -> ()) -> ()
// CHECK:         [[PA:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[RESULT]])
// CHECK:         return [[PA]]
// CHECK:       } // end sil function

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s30vtable_thunks_ownership_nested1VCIegx_ACIegg_TR : $@convention(thin) (@guaranteed V, @guaranteed @callee_guaranteed (@owned V) -> ()) -> () {
// CHECK:       bb0([[X:%.*]] : @guaranteed $V, [[G:%.*]] : @guaranteed $@callee_guaranteed (@owned V) -> ()):
// CHECK:         [[X_COPY:%.*]] = copy_value [[X]]
// CHECK:         apply [[G]]([[X_COPY]])
// CHECK-NOT:     destroy_value
// CHECK:       } // end sil function

// Dynamic dispatch through the derived class uses the override's own entry.
// CHECK-LABEL: sil hidden [ossa] @$s30vtable_thunks_ownership_nested11callDerivedyyAA0F0C_yAA1VCnXEtF : $@convention(thin) (@guaranteed Derived, @guaranteed @noescape @callee_guaranteed (@owned V) -> ()) -> () {
// CHECK:         class_method {{%.*}}, #Derived.closureParam : {{.*}}, $@convention(method) (@guaranteed @noescape @callee_guaranteed (@owned V) -> (), @guaranteed Derived) -> ()
// CHECK:       } // end sil function
func callDerived(_ d: Derived, _ g: (consuming V) -> ()) {
  d.closureParam(g)
}

// A closure parameter without an ownership specifier is already borrowed, and
// a sending parameter is already owned, so those overrides don't need a thunk
// or a new entry.
// CHECK-LABEL: sil_vtable Derived {
// CHECK-NEXT:    #Base.closureParam: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC12closureParamyyyAA1VCnXEFAA4BaseCADyyyAFXEFTV [override]
// CHECK-NEXT:    #Base.closureResult: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC13closureResultyAA1VCcyFAA4BaseCADyAFncyFTV [override]
// CHECK-NEXT:    #Base.closureProperty!getter: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC15closurePropertyyyAA1VCncvgAA4BaseCADyyAFcvgTV [override]
// CHECK-NEXT:    #Base.optionalClosureParam: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC20optionalClosureParamyyyAA1VCncSgFAA4BaseCADyyyAFcFTV [override]
// CHECK-NEXT:    #Base.closureInTuple: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC14closureInTupleyAA1VCnc_SityFAA4BaseCADyAFc_SityFTV [override]
// CHECK-NEXT:    #Base.closureParamOfClosure: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC21closureParamOfClosureyyyyAA1VCXEXEFAA4BaseCADyyyyAFnXEXEFTV [override]
// CHECK-NEXT:    #Base.generic: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC7genericyyyxnXElFAA4BaseCADyyyxXElFTV [override]
// CHECK-NEXT:    #Base.defaultToBorrow: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC15defaultToBorrowyyyAA1VCXEF [override]
// CHECK-NEXT:    #Base.consumeToSending: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC16consumeToSendingyyyAA1VCnYuXEF [override]
// CHECK-NEXT:    #Base.init!allocator: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedCACycfC [override]
// CHECK-NEXT:    #Derived.closureParam: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC12closureParamyyyAA1VCnXEF
// CHECK-NEXT:    #Derived.closureResult: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC13closureResultyAA1VCcyF
// CHECK-NEXT:    #Derived.closureProperty!getter: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC15closurePropertyyyAA1VCncvg
// CHECK-NEXT:    #Derived.optionalClosureParam: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC20optionalClosureParamyyyAA1VCncSgF
// CHECK-NEXT:    #Derived.closureInTuple: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC14closureInTupleyAA1VCnc_SityF
// CHECK-NEXT:    #Derived.closureParamOfClosure: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC21closureParamOfClosureyyyyAA1VCXEXEF
// CHECK-NEXT:    #Derived.generic: {{.*}} : @$s30vtable_thunks_ownership_nested7DerivedC7genericyyyxnXElF
// CHECK-NEXT:    #Derived.deinit!deallocator
// CHECK-NEXT:  }

// An override of the new entry also gets a thunk for the original one.
// CHECK-LABEL: sil_vtable MoreDerived {
// CHECK-NEXT:    #Base.closureParam: {{.*}} : @$s30vtable_thunks_ownership_nested11MoreDerivedC12closureParamyyyAA1VCnXEFAA4BaseCADyyyAFXEFTV [override]
// CHECK:         #Derived.closureParam: {{.*}} : @$s30vtable_thunks_ownership_nested11MoreDerivedC12closureParamyyyAA1VCnXEF [override]
// CHECK:       }
