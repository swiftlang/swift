// RUN: %target-swift-emit-silgen -module-name vtable_thunks_ownership %s | %FileCheck %s --enable-var-scope

// An override may change the ownership of its parameters. The vtable entry
// keeps the conventions of the method that introduced it, so a thunk adapts
// them whenever the lowered conventions differ.

class V {}

class Base {
  func borrowToConsume(_ x: borrowing V) {}
  func consumeToBorrow(_ x: consuming V) {}
  func defaultToConsume(_ x: V) {}
  func defaultToBorrow(_ x: V) {}
  func sharedToOwned(_ x: __shared V) {}
  func trivial(_ x: borrowing Int) {}
  func generic<T>(_ x: borrowing T) {}
  __consuming func consumingSelf() {}
  func borrowingSelf() {}
  func covariant(_ x: borrowing V) -> Base? { nil }
  func dropSending(_ x: sending V) {}
  func closureResult(_ x: borrowing V) -> (V) -> V { { $0 } }
  func optionalClosureResult(_ x: borrowing V) -> () -> Base? { { nil } }
  init(_ x: V) {}
  init(borrow x: V) {}
}

class Derived: Base {
  override func borrowToConsume(_ x: consuming V) {}
  override func consumeToBorrow(_ x: borrowing V) {}
  override func defaultToConsume(_ x: consuming V) {}
  override func defaultToBorrow(_ x: borrowing V) {}
  override func sharedToOwned(_ x: __owned V) {}
  override func trivial(_ x: consuming Int) {}
  override func generic<T>(_ x: consuming T) {}
  override func consumingSelf() {}
  override __consuming func borrowingSelf() {}
  override func covariant(_ x: consuming V) -> Derived { self }
  override func dropSending(_ x: V) {}
  override func closureResult(_ x: consuming V) -> (V) -> V { { $0 } }
  override func optionalClosureResult(_ x: consuming V) -> () -> Derived { { self } }
  override init(_ x: consuming V) { super.init(x) }
  override init(borrow x: borrowing V) { super.init(copy x) }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC15borrowToConsumeyyAA1VCnFAA4BaseCADyyAFFTV : $@convention(method) (@guaranteed V, @guaranteed Derived) -> () {
// CHECK:       bb0([[X:%.*]] : @guaranteed $V, [[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[X_COPY:%.*]] = copy_value [[X]]
// CHECK:         [[FN:%.*]] = function_ref @$s23vtable_thunks_ownership7DerivedC15borrowToConsumeyyAA1VCnF : $@convention(method) (@owned V, @guaranteed Derived) -> ()
// CHECK:         apply [[FN]]([[X_COPY]], [[SELF]])
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC15consumeToBorrowyyAA1VCFAA4BaseCADyyAFnFTV : $@convention(method) (@owned V, @guaranteed Derived) -> () {
// CHECK:       bb0([[X:%.*]] : @owned $V, [[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[X_BORROW:%.*]] = begin_borrow [[X]]
// CHECK:         [[FN:%.*]] = function_ref @$s23vtable_thunks_ownership7DerivedC15consumeToBorrowyyAA1VCF : $@convention(method) (@guaranteed V, @guaranteed Derived) -> ()
// CHECK:         apply [[FN]]([[X_BORROW]], [[SELF]])
// CHECK:         end_borrow [[X_BORROW]]
// CHECK:         destroy_value [[X]]
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC16defaultToConsumeyyAA1VCnFAA4BaseCADyyAFFTV : $@convention(method) (@guaranteed V, @guaranteed Derived) -> () {
// CHECK:         copy_value
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC13sharedToOwnedyyAA1VCnFAA4BaseCADyyAFhFTV : $@convention(method) (@guaranteed V, @guaranteed Derived) -> () {
// CHECK:         copy_value
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC7genericyyxnlFAA4BaseCADyyxlFTV : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed Derived) -> () {
// CHECK:       bb0([[X:%.*]] : $*τ_0_0, [[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[X_COPY:%.*]] = alloc_stack $τ_0_0
// CHECK:         copy_addr [[X]] to [init] [[X_COPY]]
// CHECK:         [[FN:%.*]] = function_ref @$s23vtable_thunks_ownership7DerivedC7genericyyxnlF : $@convention(method) <τ_0_0> (@in τ_0_0, @guaranteed Derived) -> ()
// CHECK:         apply [[FN]]<τ_0_0>([[X_COPY]], [[SELF]])
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC13consumingSelfyyFAA4BaseCADyyFTV : $@convention(method) (@owned Derived) -> () {
// CHECK:       bb0([[SELF:%.*]] : @owned $Derived):
// CHECK:         [[SELF_BORROW:%.*]] = begin_borrow [[SELF]]
// CHECK:         [[FN:%.*]] = function_ref @$s23vtable_thunks_ownership7DerivedC13consumingSelfyyF : $@convention(method) (@guaranteed Derived) -> ()
// CHECK:         apply [[FN]]([[SELF_BORROW]])
// CHECK:         destroy_value [[SELF]]
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC13borrowingSelfyyFAA4BaseCADyyFTV : $@convention(method) (@guaranteed Derived) -> () {
// CHECK:       bb0([[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:         [[FN:%.*]] = function_ref @$s23vtable_thunks_ownership7DerivedC13borrowingSelfyyF : $@convention(method) (@owned Derived) -> ()
// CHECK:         apply [[FN]]([[SELF_COPY]])
// CHECK-NOT:     destroy
// CHECK:       } // end sil function

// The thunk keeps the covariant result type of the override.
// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC9covariantyAcA1VCnFAA4BaseCADyAHSgAFFTV : $@convention(method) (@guaranteed V, @guaranteed Derived) -> @owned Derived {
// CHECK:         copy_value
// CHECK:       } // end sil function

// A `sending` parameter is owned.
// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC11dropSendingyyAA1VCFAA4BaseCADyyAFnFTV : $@convention(method) (@sil_sending @owned V, @guaranteed Derived) -> () {
// CHECK:       bb0([[X:%.*]] : @owned $V, [[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[X_BORROW:%.*]] = begin_borrow [[X]]
// CHECK:         [[FN:%.*]] = function_ref @$s23vtable_thunks_ownership7DerivedC11dropSendingyyAA1VCF : $@convention(method) (@guaranteed V, @guaranteed Derived) -> ()
// CHECK:         apply [[FN]]([[X_BORROW]], [[SELF]])
// CHECK:         destroy_value [[X]]
// CHECK:       } // end sil function

// The thunk adapts only the method's own parameter. A function-typed result
// is not a parameter list of the method and keeps its own conventions.
// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC13closureResultyAA1VCAFcAFnFAA4BaseCADyA2FcAFFTV : $@convention(method) (@guaranteed V, @guaranteed Derived) -> @owned @callee_guaranteed (@guaranteed V) -> @owned V {
// CHECK:       bb0([[X:%.*]] : @guaranteed $V, [[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[X_COPY:%.*]] = copy_value [[X]]
// CHECK:         [[FN:%.*]] = function_ref @$s23vtable_thunks_ownership7DerivedC13closureResultyAA1VCAFcAFnF : $@convention(method) (@owned V, @guaranteed Derived) -> @owned @callee_guaranteed (@guaranteed V) -> @owned V
// CHECK:         apply [[FN]]([[X_COPY]], [[SELF]])
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC21optionalClosureResultyACycAA1VCnFAA4BaseCADyAHSgycAFFTV : $@convention(method) (@guaranteed V, @guaranteed Derived) -> @owned @callee_guaranteed () -> @owned Derived {
// CHECK:         copy_value
// CHECK:         function_ref @$s23vtable_thunks_ownership7DerivedC21optionalClosureResultyACycAA1VCnF : $@convention(method) (@owned V, @guaranteed Derived) -> @owned @callee_guaranteed () -> @owned Derived
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership7DerivedC6borrowAcA1VCh_tcfCAA4BaseCAdhF_tcfCTV : $@convention(method) (@owned V, @thick Derived.Type) -> @owned Derived {
// CHECK:       bb0([[X:%.*]] : @owned $V, [[SELF:%.*]] : $@thick Derived.Type):
// CHECK:         [[X_BORROW:%.*]] = begin_borrow [[X]]
// CHECK:         [[FN:%.*]] = function_ref @$s23vtable_thunks_ownership7DerivedC6borrowAcA1VCh_tcfC : $@convention(method) (@guaranteed V, @thick Derived.Type) -> @owned Derived
// CHECK:         apply [[FN]]([[X_BORROW]], [[SELF]])
// CHECK:         destroy_value [[X]]
// CHECK:       } // end sil function

class MoreDerived: Derived {
  override func borrowToConsume(_ x: consuming V) {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s23vtable_thunks_ownership11MoreDerivedC15borrowToConsumeyyAA1VCnFAA4BaseCADyyAFFTV : $@convention(method) (@guaranteed V, @guaranteed MoreDerived) -> () {
// CHECK:         copy_value
// CHECK:       } // end sil function

// Dynamic dispatch through a derived class uses the conventions of the
// introducing method, but keeps the derived result type.
// CHECK-LABEL: sil hidden [ossa] @$s23vtable_thunks_ownership11callDerivedyAA0E0CAD_AA1VCtF : $@convention(thin) (@guaranteed Derived, @guaranteed V) -> @owned Derived {
// CHECK:       bb0([[D:%.*]] : @guaranteed $Derived, [[V:%.*]] : @guaranteed $V):
// CHECK:         [[M1:%.*]] = class_method [[D]], #Derived.borrowToConsume : {{.*}}, $@convention(method) (@guaranteed V, @guaranteed Derived) -> ()
// CHECK:         apply [[M1]]([[V]], [[D]])
// CHECK:         [[V_COPY:%.*]] = copy_value [[V]]
// CHECK:         [[M2:%.*]] = class_method [[D]], #Derived.consumeToBorrow : {{.*}}, $@convention(method) (@owned V, @guaranteed Derived) -> ()
// CHECK:         apply [[M2]]([[V_COPY]], [[D]])
// CHECK:         [[M3:%.*]] = class_method [[D]], #Derived.covariant : {{.*}}, $@convention(method) (@guaranteed V, @guaranteed Derived) -> @owned Derived
// CHECK:         [[R:%.*]] = apply [[M3]]([[V]], [[D]])
// CHECK:         return [[R]]
// CHECK:       } // end sil function
func callDerived(_ d: Derived, _ v: V) -> Derived {
  d.borrowToConsume(v)
  d.consumeToBorrow(v)
  return d.covariant(v)
}

// Only overrides whose lowered conventions differ go through a thunk.
// CHECK-LABEL: sil_vtable Derived {
// CHECK-NEXT:    #Base.borrowToConsume: {{.*}} : @$s23vtable_thunks_ownership7DerivedC15borrowToConsumeyyAA1VCnFAA4BaseCADyyAFFTV [override]
// CHECK-NEXT:    #Base.consumeToBorrow: {{.*}} : @$s23vtable_thunks_ownership7DerivedC15consumeToBorrowyyAA1VCFAA4BaseCADyyAFnFTV [override]
// CHECK-NEXT:    #Base.defaultToConsume: {{.*}} : @$s23vtable_thunks_ownership7DerivedC16defaultToConsumeyyAA1VCnFAA4BaseCADyyAFFTV [override]
// CHECK-NEXT:    #Base.defaultToBorrow: {{.*}} : @$s23vtable_thunks_ownership7DerivedC15defaultToBorrowyyAA1VCF [override]
// CHECK-NEXT:    #Base.sharedToOwned: {{.*}} : @$s23vtable_thunks_ownership7DerivedC13sharedToOwnedyyAA1VCnFAA4BaseCADyyAFhFTV [override]
// CHECK-NEXT:    #Base.trivial: {{.*}} : @$s23vtable_thunks_ownership7DerivedC7trivialyySinF [override]
// CHECK-NEXT:    #Base.generic: {{.*}} : @$s23vtable_thunks_ownership7DerivedC7genericyyxnlFAA4BaseCADyyxlFTV [override]
// CHECK-NEXT:    #Base.consumingSelf: {{.*}} : @$s23vtable_thunks_ownership7DerivedC13consumingSelfyyFAA4BaseCADyyFTV [override]
// CHECK-NEXT:    #Base.borrowingSelf: {{.*}} : @$s23vtable_thunks_ownership7DerivedC13borrowingSelfyyFAA4BaseCADyyFTV [override]
// CHECK-NEXT:    #Base.covariant: {{.*}} : @$s23vtable_thunks_ownership7DerivedC9covariantyAcA1VCnFAA4BaseCADyAHSgAFFTV [override]
// CHECK-NEXT:    #Base.dropSending: {{.*}} : @$s23vtable_thunks_ownership7DerivedC11dropSendingyyAA1VCFAA4BaseCADyyAFnFTV [override]
// CHECK-NEXT:    #Base.closureResult: {{.*}} : @$s23vtable_thunks_ownership7DerivedC13closureResultyAA1VCAFcAFnFAA4BaseCADyA2FcAFFTV [override]
// CHECK-NEXT:    #Base.optionalClosureResult: {{.*}} : @$s23vtable_thunks_ownership7DerivedC21optionalClosureResultyACycAA1VCnFAA4BaseCADyAHSgycAFFTV [override]
// CHECK-NEXT:    #Base.init!allocator: {{.*}} : @$s23vtable_thunks_ownership7DerivedCyAcA1VCcfC [override]
// CHECK-NEXT:    #Base.init!allocator: {{.*}} : @$s23vtable_thunks_ownership7DerivedC6borrowAcA1VCh_tcfCAA4BaseCAdhF_tcfCTV [override]
// CHECK-NEXT:    #Derived.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable MoreDerived {
// CHECK-NEXT:    #Base.borrowToConsume: {{.*}} : @$s23vtable_thunks_ownership11MoreDerivedC15borrowToConsumeyyAA1VCnFAA4BaseCADyyAFFTV [override]
// CHECK-NEXT:    #Base.consumeToBorrow: {{.*}} : @$s23vtable_thunks_ownership7DerivedC15consumeToBorrowyyAA1VCFAA4BaseCADyyAFnFTV [inherited]
