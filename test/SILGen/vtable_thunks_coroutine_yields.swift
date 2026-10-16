// RUN: %target-swift-emit-silgen -enable-experimental-feature CoroutineAccessors -module-name vtable_thunks_coroutine_yields %s | %FileCheck %s --enable-var-scope
// RUN: %target-swift-emit-silgen -enable-experimental-feature CoroutineAccessors -enable-library-evolution -module-name vtable_thunks_coroutine_yields %s | %FileCheck %s --check-prefixes=CHECK,RESILIENT --enable-var-scope

// REQUIRES: swift_feature_CoroutineAccessors

// An override of a read coroutine may yield a different type than the
// coroutine it overrides. When the yields aren't ABI-compatible, the override
// gets its own vtable entry, and the entry it overrides gets a thunk that
// translates the yield.

public class V {}
public struct NC: ~Copyable { public init() {} }

open class Base {
  open var number: Int? { _read { yield nil } }
  open var text: String? { _read { yield nil } }
  open subscript(i: Int) -> Int? { _read { yield nil } }
  open var closure: (borrowing V) -> () { _read { yield { _ in } } }
  open var covariant: Base { _read { yield self } }
}

open class Derived: Base {
  open override var number: Int { _read { yield 0 } }
  open override var text: String { _read { yield "" } }
  open override subscript(i: Int) -> Int { _read { yield i } }
  open override var closure: (consuming V) -> () { _read { yield { _ in } } }
  open override var covariant: Derived { _read { yield self } }
}

open class GenericBase<T> {
  open var value: T? { _read { yield nil } }
}

open class ConcreteDerived: GenericBase<Int> {
  open override var value: Int { _read { yield 0 } }
}

open class GenericDerived<U>: GenericBase<U> {
  public let stored: U
  public init(_ stored: U) { self.stored = stored }
  open override var value: U { _read { yield stored } }
}

open class NoncopyableBase {
  open var nc: NC? { _read { yield nil } }
}

open class NoncopyableDerived: NoncopyableBase {
  public let stored = NC()
  open override var nc: NC { _read { yield stored } }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s30vtable_thunks_coroutine_yields7DerivedC4textSSvyAA4BaseCADSSSgvyTV : $@yield_once_2 @convention(method) (@guaranteed Derived) -> @yields @guaranteed Optional<String> {
// CHECK:       bb0([[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[FN:%.*]] = function_ref @$s30vtable_thunks_coroutine_yields7DerivedC4textSSvy : $@yield_once_2 @convention(method) (@guaranteed Derived) -> @yields @guaranteed String
// CHECK:         ([[TEXT:%.*]], [[TOKEN:%.*]], {{%.*}}) = begin_apply [[FN]]([[SELF]])
// CHECK:         [[SOME:%.*]] = enum $Optional<String>, #Optional.some!enumelt, [[TEXT]]
// CHECK:         yield [[SOME]], resume [[RESUME:bb[0-9]+]], unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[RESUME]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:       [[UNWIND]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         unwind
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s30vtable_thunks_coroutine_yields7DerivedC7closureyyAA1VCncvyAA4BaseCADyyAFcvyTV : $@yield_once_2 @convention(method) (@guaranteed Derived) -> @yields @guaranteed @callee_guaranteed (@guaranteed V) -> () {
// CHECK:       bb0([[SELF:%.*]] : @guaranteed $Derived):
// CHECK:         [[FN:%.*]] = function_ref @$s30vtable_thunks_coroutine_yields7DerivedC7closureyyAA1VCncvy : $@yield_once_2 @convention(method) (@guaranteed Derived) -> @yields @guaranteed @callee_guaranteed (@owned V) -> ()
// CHECK:         ([[CLOSURE:%.*]], [[TOKEN:%.*]], {{%.*}}) = begin_apply [[FN]]([[SELF]])
// CHECK:         [[CLOSURE_COPY:%.*]] = copy_value [[CLOSURE]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$s30vtable_thunks_coroutine_yields1VCIegx_ACIegg_TR : $@convention(thin) (@guaranteed V, @guaranteed @callee_guaranteed (@owned V) -> ()) -> ()
// CHECK:         [[PA:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[CLOSURE_COPY]])
// CHECK:         [[BORROW:%.*]] = begin_borrow [[PA]]
// CHECK:         yield [[BORROW]], resume [[RESUME:bb[0-9]+]], unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[RESUME]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[PA]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:       [[UNWIND]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[PA]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         unwind
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s30vtable_thunks_coroutine_yields14GenericDerivedC5valuexvyAA0E4BaseCADxSgvyTV : $@yield_once_2 @convention(method) <τ_0_0> @substituted <τ_0_0, τ_0_1> (@guaranteed GenericDerived<τ_0_0>) -> @yields @in_guaranteed Optional<τ_0_1> for <τ_0_0, τ_0_0> {
// CHECK:       bb0([[SELF:%.*]] : @guaranteed $GenericDerived<τ_0_0>):
// CHECK:         [[FN:%.*]] = function_ref @$s30vtable_thunks_coroutine_yields14GenericDerivedC5valuexvy : $@yield_once_2 @convention(method) <τ_0_0> (@guaranteed GenericDerived<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:         ([[VALUE:%.*]], [[TOKEN:%.*]], {{%.*}}) = begin_apply [[FN]]<τ_0_0>([[SELF]])
// CHECK:         [[OPT:%.*]] = alloc_stack $Optional<τ_0_0>
// CHECK:         [[PAYLOAD:%.*]] = init_enum_data_addr [[OPT]], #Optional.some!enumelt
// CHECK:         [[TMP:%.*]] = alloc_stack $τ_0_0
// CHECK:         copy_addr [[VALUE]] to [init] [[TMP]]
// CHECK:         copy_addr [take] [[TMP]] to [init] [[PAYLOAD]]
// CHECK:         inject_enum_addr [[OPT]], #Optional.some!enumelt
// CHECK:         yield [[OPT]], resume [[RESUME:bb[0-9]+]], unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[RESUME]]:
// CHECK:         destroy_addr [[OPT]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:       [[UNWIND]]:
// CHECK:         destroy_addr [[OPT]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:       } // end sil function

// CHECK-LABEL: sil private [thunk] [ossa] @$s30vtable_thunks_coroutine_yields18NoncopyableDerivedC2ncAA2NCVvyAA0E4BaseCAdFSgvyTV : $@yield_once_2 @convention(method) (@guaranteed NoncopyableDerived) -> @yields {{@guaranteed|@in_guaranteed}} Optional<NC> {
// CHECK:         begin_apply
// CHECK-NOT:     copy_
// CHECK:         enum $Optional<NC>, #Optional.some!enumelt
// CHECK-NOT:     copy_
// CHECK:         yield
// CHECK:       } // end sil function

// A class-covariant yield is ABI-compatible, so that override needs neither a
// thunk nor an entry of its own.
// CHECK-LABEL: sil_vtable {{(\[serialized\] )?}}Derived {
// RESILIENT:     #Base.number!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC6numberSivrAA4BaseCADSiSgvrTV [override]
// CHECK:         #Base.number!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC6numberSivyAA4BaseCADSiSgvyTV [override]
// RESILIENT:     #Base.text!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC4textSSvrAA4BaseCADSSSgvrTV [override]
// CHECK:         #Base.text!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC4textSSvyAA4BaseCADSSSgvyTV [override]
// RESILIENT:     #Base.subscript!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedCyS2icirAA4BaseCySiSgSicirTV [override]
// CHECK:         #Base.subscript!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedCyS2iciyAA4BaseCySiSgSiciyTV [override]
// RESILIENT:     #Base.closure!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC7closureyyAA1VCncvrAA4BaseCADyyAFcvrTV [override]
// CHECK:         #Base.closure!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC7closureyyAA1VCncvyAA4BaseCADyyAFcvyTV [override]
// RESILIENT:     #Base.covariant!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC9covariantACvr [override]
// CHECK:         #Base.covariant!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC9covariantACvy [override]
// RESILIENT:     #Derived.number!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC6numberSivr
// CHECK:         #Derived.number!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC6numberSivy
// RESILIENT:     #Derived.text!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC4textSSvr
// CHECK:         #Derived.text!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC4textSSvy
// RESILIENT:     #Derived.subscript!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedCyS2icir
// CHECK:         #Derived.subscript!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedCyS2iciy
// RESILIENT:     #Derived.closure!read: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC7closureyyAA1VCncvr
// CHECK:         #Derived.closure!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields7DerivedC7closureyyAA1VCncvy
// CHECK-NOT:     #Derived.covariant
// CHECK:       }

// CHECK-LABEL: sil_vtable {{(\[serialized\] )?}}ConcreteDerived {
// RESILIENT:     #GenericBase.value!read: {{.*}} : @$s30vtable_thunks_coroutine_yields15ConcreteDerivedC5valueSivrAA11GenericBaseCADxSgvrTV [override]
// CHECK:         #GenericBase.value!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields15ConcreteDerivedC5valueSivyAA11GenericBaseCADxSgvyTV [override]
// RESILIENT:     #ConcreteDerived.value!read: {{.*}} : @$s30vtable_thunks_coroutine_yields15ConcreteDerivedC5valueSivr
// CHECK:         #ConcreteDerived.value!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields15ConcreteDerivedC5valueSivy
// CHECK:       }

// CHECK-LABEL: sil_vtable {{(\[serialized\] )?}}GenericDerived {
// RESILIENT:     #GenericBase.value!read: {{.*}} : @$s30vtable_thunks_coroutine_yields14GenericDerivedC5valuexvrAA0E4BaseCADxSgvrTV [override]
// CHECK:         #GenericBase.value!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields14GenericDerivedC5valuexvyAA0E4BaseCADxSgvyTV [override]
// RESILIENT:     #GenericDerived.value!read: {{.*}} : @$s30vtable_thunks_coroutine_yields14GenericDerivedC5valuexvr
// CHECK:         #GenericDerived.value!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields14GenericDerivedC5valuexvy
// CHECK:       }

// CHECK-LABEL: sil_vtable {{(\[serialized\] )?}}NoncopyableDerived {
// RESILIENT:     #NoncopyableBase.nc!read: {{.*}} : @$s30vtable_thunks_coroutine_yields18NoncopyableDerivedC2ncAA2NCVvrAA0E4BaseCAdFSgvrTV [override]
// CHECK:         #NoncopyableBase.nc!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields18NoncopyableDerivedC2ncAA2NCVvyAA0E4BaseCAdFSgvyTV [override]
// RESILIENT:     #NoncopyableDerived.nc!read: {{.*}} : @$s30vtable_thunks_coroutine_yields18NoncopyableDerivedC2ncAA2NCVvr
// CHECK:         #NoncopyableDerived.nc!yielding_borrow: {{.*}} : @$s30vtable_thunks_coroutine_yields18NoncopyableDerivedC2ncAA2NCVvy
// CHECK:       }
