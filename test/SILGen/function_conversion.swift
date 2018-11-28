
// RUN: %target-swift-emit-silgen -module-name function_conversion -enable-sil-ownership -primary-file %s | %FileCheck %s
// RUN: %target-swift-emit-ir -module-name function_conversion -enable-sil-ownership -primary-file %s

// Check SILGen against various FunctionConversionExprs emitted by Sema.

// ==== Representation conversions

// CHECK-LABEL: sil hidden @$s19function_conversion7cToFuncyS2icS2iXCF : $@convention(thin) (@convention(c) (Int) -> Int) -> @owned @callee_guaranteed (Int) -> Int
// CHECK:         [[THUNK:%.*]] = function_ref @$sS2iIetCyd_S2iIegyd_TR
// CHECK:         [[FUNC:%.*]] = partial_apply [callee_guaranteed] [[THUNK]](%0)
// CHECK:         return [[FUNC]]
func cToFunc(_ arg: @escaping @convention(c) (Int) -> Int) -> (Int) -> Int {
  return arg
}

// CHECK-LABEL: sil hidden @$s19function_conversion8cToBlockyS2iXBS2iXCF : $@convention(thin) (@convention(c) (Int) -> Int) -> @owned @convention(block) (Int) -> Int
// CHECK:         [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// CHECK:         [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]]
// CHECK:         [[COPY:%.*]] = copy_block [[BLOCK]] : $@convention(block) (Int) -> Int
// CHECK:         return [[COPY]]
func cToBlock(_ arg: @escaping @convention(c) (Int) -> Int) -> @convention(block) (Int) -> Int {
  return arg
}

// ==== Throws variance

// CHECK-LABEL: sil hidden @$s19function_conversion12funcToThrowsyyyKcyycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @owned @callee_guaranteed () -> @error Error
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed () -> ()):
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   [[FUNC:%.*]] = convert_function [[ARG_COPY]] : $@callee_guaranteed () -> () to $@callee_guaranteed () -> @error Error
// CHECK:   return [[FUNC]]
// CHECK: } // end sil function '$s19function_conversion12funcToThrowsyyyKcyycF'
func funcToThrows(_ x: @escaping () -> ()) -> () throws -> () {
  return x
}

// CHECK-LABEL: sil hidden @$s19function_conversion12thinToThrowsyyyKXfyyXfF : $@convention(thin) (@convention(thin) () -> ()) -> @convention(thin) () -> @error Error
// CHECK:         [[FUNC:%.*]] = convert_function %0 : $@convention(thin) () -> () to $@convention(thin) () -> @error Error
// CHECK:         return [[FUNC]] : $@convention(thin) () -> @error Error
func thinToThrows(_ x: @escaping @convention(thin) () -> ()) -> @convention(thin) () throws -> () {
  return x
}

// FIXME: triggers an assert because we always do a thin to thick conversion on DeclRefExprs
/*
func thinFunc() {}

func thinToThrows() {
  let _: @convention(thin) () -> () = thinFunc
}
*/

// ==== Class downcasts and upcasts

class Feral {}
class Domesticated : Feral {}

// CHECK-LABEL: sil hidden @$s19function_conversion12funcToUpcastyAA5FeralCycAA12DomesticatedCycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> @owned Domesticated) -> @owned @callee_guaranteed () -> @owned Feral {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed () -> @owned Domesticated):
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   [[FUNC:%.*]] = convert_function [[ARG_COPY]] : $@callee_guaranteed () -> @owned Domesticated to $@callee_guaranteed () -> @owned Feral
// CHECK:   return [[FUNC]]
// CHECK: } // end sil function '$s19function_conversion12funcToUpcastyAA5FeralCycAA12DomesticatedCycF'
func funcToUpcast(_ x: @escaping () -> Domesticated) -> () -> Feral {
  return x
}

// CHECK-LABEL: sil hidden @$s19function_conversion12funcToUpcastyyAA12DomesticatedCcyAA5FeralCcF : $@convention(thin) (@guaranteed @callee_guaranteed (@guaranteed Feral) -> ()) -> @owned @callee_guaranteed (@guaranteed Domesticated) -> ()
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   [[FUNC:%.*]] = convert_function [[ARG_COPY]] : $@callee_guaranteed (@guaranteed Feral) -> () to $@callee_guaranteed (@guaranteed Domesticated) -> (){{.*}}
// CHECK:   return [[FUNC]]
func funcToUpcast(_ x: @escaping (Feral) -> ()) -> (Domesticated) -> () {
  return x
}

// ==== Optionals

struct Trivial {
  let n: Int8
}

class C {
  let n: Int8

  init(n: Int8) {
    self.n = n
  }
}

struct Loadable {
  let c: C

  var n: Int8 {
    return c.n
  }

  init(n: Int8) {
    c = C(n: n)
  }
}

struct AddrOnly {
  let a: Any

  var n: Int8 {
    return a as! Int8
  }

  init(n: Int8) {
    a = n
  }
}

// CHECK-LABEL: sil hidden @$s19function_conversion19convOptionalTrivialyyAA0E0VADSgcF
func convOptionalTrivial(_ t1: @escaping (Trivial?) -> Trivial) {
// CHECK:         function_ref @$s19function_conversion7TrivialVSgACIegyd_AcDIegyd_TR
// CHECK:         partial_apply
  let _: (Trivial) -> Trivial? = t1

// CHECK:         function_ref @$s19function_conversion7TrivialVSgACIegyd_A2DIegyd_TR
// CHECK:         partial_apply
  let _: (Trivial?) -> Trivial? = t1
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion7TrivialVSgACIegyd_AcDIegyd_TR : $@convention(thin) (Trivial, @guaranteed @callee_guaranteed (Optional<Trivial>) -> Trivial) -> Optional<Trivial>
// CHECK:         [[ENUM:%.*]] = enum $Optional<Trivial>
// CHECK-NEXT:    apply %1([[ENUM]])
// CHECK-NEXT:    enum $Optional<Trivial>
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion7TrivialVSgACIegyd_A2DIegyd_TR : $@convention(thin) (Optional<Trivial>, @guaranteed @callee_guaranteed (Optional<Trivial>) -> Trivial) -> Optional<Trivial>
// CHECK:         apply %1(%0)
// CHECK-NEXT:    enum $Optional<Trivial>
// CHECK-NEXT:    return

// CHECK-LABEL: sil hidden @$s19function_conversion20convOptionalLoadableyyAA0E0VADSgcF
func convOptionalLoadable(_ l1: @escaping (Loadable?) -> Loadable) {
// CHECK:         function_ref @$s19function_conversion8LoadableVSgACIeggo_AcDIeggo_TR
// CHECK:         partial_apply
  let _: (Loadable) -> Loadable? = l1

// CHECK:         function_ref @$s19function_conversion8LoadableVSgACIeggo_A2DIeggo_TR
// CHECK:         partial_apply
  let _: (Loadable?) -> Loadable? = l1
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion8LoadableVSgACIeggo_A2DIeggo_TR : $@convention(thin) (@guaranteed Optional<Loadable>, @guaranteed @callee_guaranteed (@guaranteed Optional<Loadable>) -> @owned Loadable) -> @owned Optional<Loadable>
// CHECK:         apply %1(%0)
// CHECK-NEXT:    enum $Optional<Loadable>
// CHECK-NEXT:    return

// CHECK-LABEL: sil hidden @$s19function_conversion20convOptionalAddrOnlyyyAA0eF0VADSgcF
func convOptionalAddrOnly(_ a1: @escaping (AddrOnly?) -> AddrOnly) {
// CHECK:         function_ref @$s19function_conversion8AddrOnlyVSgACIegnr_A2DIegnr_TR
// CHECK:         partial_apply
  let _: (AddrOnly?) -> AddrOnly? = a1
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion8AddrOnlyVSgACIegnr_A2DIegnr_TR : $@convention(thin) (@in_guaranteed Optional<AddrOnly>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<AddrOnly>) -> @out AddrOnly) -> @out Optional<AddrOnly>
// CHECK:         [[TEMP:%.*]] = alloc_stack $AddrOnly
// CHECK-NEXT:    apply %2([[TEMP]], %1)
// CHECK-NEXT:    init_enum_data_addr %0 : $*Optional<AddrOnly>
// CHECK-NEXT:    copy_addr [take] {{.*}} to [initialization] {{.*}} : $*AddrOnly
// CHECK-NEXT:    inject_enum_addr %0 : $*Optional<AddrOnly>
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    dealloc_stack {{.*}} : $*AddrOnly
// CHECK-NEXT:    return

// ==== Existentials

protocol Q {
  var n: Int8 { get }
}

protocol P : Q {}

extension Trivial : P {}
extension Loadable : P {}
extension AddrOnly : P {}

// CHECK-LABEL: sil hidden @$s19function_conversion22convExistentialTrivial_2t3yAA0E0VAA1Q_pc_AeaF_pSgctF
func convExistentialTrivial(_ t2: @escaping (Q) -> Trivial, t3: @escaping (Q?) -> Trivial) {
// CHECK:         function_ref @$s19function_conversion1Q_pAA7TrivialVIegnd_AdA1P_pIegyr_TR
// CHECK:         partial_apply
  let _: (Trivial) -> P = t2

// CHECK:         function_ref @$s19function_conversion1Q_pSgAA7TrivialVIegnd_AESgAA1P_pIegyr_TR
// CHECK:         partial_apply
  let _: (Trivial?) -> P = t3

// CHECK:         function_ref @$s19function_conversion1Q_pAA7TrivialVIegnd_AA1P_pAaE_pIegnr_TR
// CHECK:         partial_apply
  let _: (P) -> P = t2
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion1Q_pAA7TrivialVIegnd_AdA1P_pIegyr_TR : $@convention(thin) (Trivial, @guaranteed @callee_guaranteed (@in_guaranteed Q) -> Trivial) -> @out P
// CHECK:         alloc_stack $Q
// CHECK-NEXT:    init_existential_addr
// CHECK-NEXT:    store
// CHECK-NEXT:    apply
// CHECK-NEXT:    init_existential_addr
// CHECK-NEXT:    store
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion1Q_pSgAA7TrivialVIegnd_AESgAA1P_pIegyr_TR
// CHECK:         switch_enum
// CHECK: bb1([[TRIVIAL:%.*]] : $Trivial):
// CHECK:         init_existential_addr
// CHECK:         init_enum_data_addr
// CHECK:         copy_addr
// CHECK:         inject_enum_addr
// CHECK: bb2:
// CHECK:         inject_enum_addr
// CHECK: bb3:
// CHECK:         apply
// CHECK:         init_existential_addr
// CHECK:         store
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion1Q_pAA7TrivialVIegnd_AA1P_pAaE_pIegnr_TR : $@convention(thin) (@in_guaranteed P, @guaranteed @callee_guaranteed (@in_guaranteed Q) -> Trivial) -> @out P
// CHECK:         [[TMP:%.*]] = alloc_stack $Q
// CHECK-NEXT:    open_existential_addr immutable_access %1 : $*P
// CHECK-NEXT:    init_existential_addr [[TMP]] : $*Q
// CHECK-NEXT:    copy_addr {{.*}} to [initialization] {{.*}}
// CHECK-NEXT:    apply
// CHECK-NEXT:    init_existential_addr
// CHECK-NEXT:    store
// CHECK:         destroy_addr
// CHECK:         return

// ==== Existential metatypes

// CHECK-LABEL: sil hidden @$s19function_conversion23convExistentialMetatypeyyAA7TrivialVmAA1Q_pXpSgcF
func convExistentialMetatype(_ em: @escaping (Q.Type?) -> Trivial.Type) {
// CHECK:         function_ref @$s19function_conversion1Q_pXmTSgAA7TrivialVXMtIegyd_AEXMtAA1P_pXmTIegyd_TR
// CHECK:         partial_apply
  let _: (Trivial.Type) -> P.Type = em

// CHECK:         function_ref @$s19function_conversion1Q_pXmTSgAA7TrivialVXMtIegyd_AEXMtSgAA1P_pXmTIegyd_TR
// CHECK:         partial_apply
  let _: (Trivial.Type?) -> P.Type = em

// CHECK:         function_ref @$s19function_conversion1Q_pXmTSgAA7TrivialVXMtIegyd_AA1P_pXmTAaF_pXmTIegyd_TR
// CHECK:         partial_apply
  let _: (P.Type) -> P.Type = em
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion1Q_pXmTSgAA7TrivialVXMtIegyd_AEXMtAA1P_pXmTIegyd_TR : $@convention(thin) (@thin Trivial.Type, @guaranteed @callee_guaranteed (Optional<@thick Q.Type>) -> @thin Trivial.Type) -> @thick P.Type
// CHECK:         [[META:%.*]] = metatype $@thick Trivial.Type
// CHECK-NEXT:    init_existential_metatype [[META]] : $@thick Trivial.Type, $@thick Q.Type
// CHECK-NEXT:    enum $Optional<@thick Q.Type>
// CHECK-NEXT:    apply
// CHECK-NEXT:    metatype $@thick Trivial.Type
// CHECK-NEXT:    init_existential_metatype {{.*}} : $@thick Trivial.Type, $@thick P.Type
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion1Q_pXmTSgAA7TrivialVXMtIegyd_AEXMtSgAA1P_pXmTIegyd_TR : $@convention(thin) (Optional<@thin Trivial.Type>, @guaranteed @callee_guaranteed (Optional<@thick Q.Type>) -> @thin Trivial.Type) -> @thick P.Type
// CHECK:         switch_enum %0 : $Optional<@thin Trivial.Type>
// CHECK: bb1([[META:%.*]] : $@thin Trivial.Type):
// CHECK-NEXT:    metatype $@thick Trivial.Type
// CHECK-NEXT:    init_existential_metatype {{.*}} : $@thick Trivial.Type, $@thick Q.Type
// CHECK-NEXT:    enum $Optional<@thick Q.Type>
// CHECK: bb2:
// CHECK-NEXT:    enum $Optional<@thick Q.Type>
// CHECK: bb3({{.*}}):
// CHECK-NEXT:    apply
// CHECK-NEXT:    metatype $@thick Trivial.Type
// CHECK-NEXT:    init_existential_metatype {{.*}} : $@thick Trivial.Type, $@thick P.Type
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion1Q_pXmTSgAA7TrivialVXMtIegyd_AA1P_pXmTAaF_pXmTIegyd_TR : $@convention(thin) (@thick P.Type, @guaranteed @callee_guaranteed (Optional<@thick Q.Type>) -> @thin Trivial.Type) -> @thick P.Type
// CHECK:         open_existential_metatype %0 : $@thick P.Type to $@thick (@opened({{.*}}) P).Type
// CHECK-NEXT:    init_existential_metatype %2 : $@thick (@opened({{.*}}) P).Type, $@thick Q.Type
// CHECK-NEXT:    enum $Optional<@thick Q.Type>
// CHECK-NEXT:    apply %1
// CHECK-NEXT:    metatype $@thick Trivial.Type
// CHECK-NEXT:    init_existential_metatype {{.*}} : $@thick Trivial.Type, $@thick P.Type
// CHECK-NEXT:    return

// ==== Class metatype upcasts

class Parent {}
class Child : Parent {}

// Note: we add a Trivial => Trivial? conversion here to force a thunk
// to be generated

// CHECK-LABEL: sil hidden @$s19function_conversion18convUpcastMetatype_2c5yAA5ChildCmAA6ParentCm_AA7TrivialVSgtc_AEmAGmSg_AJtctF
func convUpcastMetatype(_ c4: @escaping (Parent.Type, Trivial?) -> Child.Type,
                        c5: @escaping (Parent.Type?, Trivial?) -> Child.Type) {
// CHECK:         function_ref @$s19function_conversion6ParentCXMTAA7TrivialVSgAA5ChildCXMTIegyyd_AHXMTAeCXMTIegyyd_TR
// CHECK:         partial_apply
  let _: (Child.Type, Trivial) -> Parent.Type = c4

// CHECK:         function_ref @$s19function_conversion6ParentCXMTSgAA7TrivialVSgAA5ChildCXMTIegyyd_AIXMTAfCXMTIegyyd_TR
// CHECK:         partial_apply
  let _: (Child.Type, Trivial) -> Parent.Type = c5

// CHECK:         function_ref @$s19function_conversion6ParentCXMTSgAA7TrivialVSgAA5ChildCXMTIegyyd_AIXMTSgAfDIegyyd_TR
// CHECK:         partial_apply
  let _: (Child.Type?, Trivial) -> Parent.Type? = c5
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion6ParentCXMTAA7TrivialVSgAA5ChildCXMTIegyyd_AHXMTAeCXMTIegyyd_TR : $@convention(thin) (@thick Child.Type, Trivial, @guaranteed @callee_guaranteed (@thick Parent.Type, Optional<Trivial>) -> @thick Child.Type) -> @thick Parent.Type
// CHECK:         upcast %0 : $@thick Child.Type to $@thick Parent.Type
// CHECK:         apply
// CHECK:         upcast {{.*}} : $@thick Child.Type to $@thick Parent.Type
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion6ParentCXMTSgAA7TrivialVSgAA5ChildCXMTIegyyd_AIXMTAfCXMTIegyyd_TR : $@convention(thin) (@thick Child.Type, Trivial, @guaranteed @callee_guaranteed (Optional<@thick Parent.Type>, Optional<Trivial>) -> @thick Child.Type) -> @thick Parent.Type
// CHECK:         upcast %0 : $@thick Child.Type to $@thick Parent.Type
// CHECK:         enum $Optional<@thick Parent.Type>
// CHECK:         apply
// CHECK:         upcast {{.*}} : $@thick Child.Type to $@thick Parent.Type
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion6ParentCXMTSgAA7TrivialVSgAA5ChildCXMTIegyyd_AIXMTSgAfDIegyyd_TR : $@convention(thin) (Optional<@thick Child.Type>, Trivial, @guaranteed @callee_guaranteed (Optional<@thick Parent.Type>, Optional<Trivial>) -> @thick Child.Type) -> Optional<@thick Parent.Type>
// CHECK:         unchecked_trivial_bit_cast %0 : $Optional<@thick Child.Type> to $Optional<@thick Parent.Type>
// CHECK:         apply
// CHECK:         upcast {{.*}} : $@thick Child.Type to $@thick Parent.Type
// CHECK:         enum $Optional<@thick Parent.Type>
// CHECK:         return

// ==== Function to existential -- make sure we maximally abstract it

// CHECK-LABEL: sil hidden @$s19function_conversion19convFuncExistentialyyS2icypcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed Any) -> @owned @callee_guaranteed (Int) -> Int) -> ()
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   [[REABSTRACT_THUNK:%.*]] = function_ref @$sypS2iIegyd_Iegno_S2iIegyd_ypIeggr_TR :
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT_THUNK]]([[ARG_COPY]])
// CHECK:   destroy_value [[PA]]
// CHECK: } // end sil function '$s19function_conversion19convFuncExistentialyyS2icypcF'
func convFuncExistential(_ f1: @escaping (Any) -> (Int) -> Int) {
  let _: (@escaping (Int) -> Int) -> Any = f1
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypS2iIegyd_Iegno_S2iIegyd_ypIeggr_TR : $@convention(thin) (@guaranteed @callee_guaranteed (Int) -> Int, @guaranteed @callee_guaranteed (@in_guaranteed Any) -> @owned @callee_guaranteed (Int) -> Int) -> @out Any {
// CHECK:         alloc_stack $Any
// CHECK:         function_ref @$sS2iIegyd_S2iIegnr_TR
// CHECK-NEXT:    [[COPIED_VAL:%.*]] = copy_value
// CHECK-NEXT:    partial_apply [callee_guaranteed] {{%.*}}([[COPIED_VAL]])
// CHECK-NEXT:    init_existential_addr %3 : $*Any, $(Int) -> Int
// CHECK-NEXT:    store
// CHECK-NEXT:    apply
// CHECK:         function_ref @$sS2iIegyd_S2iIegnr_TR
// CHECK-NEXT:    partial_apply
// CHECK-NEXT:    init_existential_addr %0 : $*Any, $(Int) -> Int
// CHECK-NEXT:    store {{.*}} to {{.*}} : $*@callee_guaranteed (@in_guaranteed Int) -> @out Int
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sS2iIegyd_S2iIegnr_TR : $@convention(thin) (@in_guaranteed Int, @guaranteed @callee_guaranteed (Int) -> Int) -> @out Int
// CHECK:         [[LOADED:%.*]] = load [trivial] %1 : $*Int
// CHECK-NEXT:    apply %2([[LOADED]])
// CHECK-NEXT:    store {{.*}} to [trivial] %0
// CHECK-NEXT:    [[VOID:%.*]] = tuple ()
// CHECK:         return [[VOID]]

// ==== Class-bound archetype upcast

// CHECK-LABEL: sil hidden @$s19function_conversion29convClassBoundArchetypeUpcast{{[_0-9a-zA-Z]*}}F
func convClassBoundArchetypeUpcast<T : Parent>(_ f1: @escaping (Parent) -> (T, Trivial)) {
// CHECK:         function_ref @$s19function_conversion6ParentCxAA7TrivialVIeggod_xAcESgIeggod_ACRbzlTR
// CHECK:         partial_apply
  let _: (T) -> (Parent, Trivial?) = f1
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion6ParentCxAA7TrivialVIeggod_xAcESgIeggod_ACRbzlTR : $@convention(thin) <T where T : Parent> (@guaranteed T, @guaranteed @callee_guaranteed (@guaranteed Parent) -> (@owned T, Trivial)) -> (@owned Parent, Optional<Trivial>)
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T, [[CLOSURE:%.*]] : @guaranteed $@callee_guaranteed (@guaranteed Parent) -> (@owned T, Trivial)):
// CHECK:    [[CASTED_ARG:%.*]] = upcast [[ARG]] : $T to $Parent
// CHECK:    [[RESULT:%.*]] = apply %1([[CASTED_ARG]])
// CHECK:    ([[LHS:%.*]], [[RHS:%.*]]) = destructure_tuple [[RESULT]]
// CHECK:    [[LHS_CAST:%.*]] = upcast [[LHS]] : $T to $Parent
// CHECK:    [[RHS_OPT:%.*]] = enum $Optional<Trivial>, #Optional.some!enumelt.1, [[RHS]]
// CHECK:    [[RESULT:%.*]] = tuple ([[LHS_CAST]] : $Parent, [[RHS_OPT]] : $Optional<Trivial>)
// CHECK:    return [[RESULT]]
// CHECK: } // end sil function '$s19function_conversion6ParentCxAA7TrivialVIeggod_xAcESgIeggod_ACRbzlTR'

// CHECK-LABEL: sil hidden @$s19function_conversion37convClassBoundMetatypeArchetypeUpcast{{[_0-9a-zA-Z]*}}F
func convClassBoundMetatypeArchetypeUpcast<T : Parent>(_ f1: @escaping (Parent.Type) -> (T.Type, Trivial)) {
// CHECK:         function_ref @$s19function_conversion6ParentCXMTxXMTAA7TrivialVIegydd_xXMTACXMTAESgIegydd_ACRbzlTR
// CHECK:         partial_apply
  let _: (T.Type) -> (Parent.Type, Trivial?) = f1
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion6ParentCXMTxXMTAA7TrivialVIegydd_xXMTACXMTAESgIegydd_ACRbzlTR : $@convention(thin) <T where T : Parent> (@thick T.Type, @guaranteed @callee_guaranteed (@thick Parent.Type) -> (@thick T.Type, Trivial)) -> (@thick Parent.Type, Optional<Trivial>)
// CHECK: bb0([[META:%.*]] : 
// CHECK:         upcast %0 : $@thick T.Type
// CHECK-NEXT:    apply
// CHECK-NEXT:    destructure_tuple
// CHECK-NEXT:    upcast {{.*}} : $@thick T.Type to $@thick Parent.Type
// CHECK-NEXT:    enum $Optional<Trivial>
// CHECK-NEXT:    tuple
// CHECK-NEXT:    return

// ==== Make sure we destructure one-element tuples

// CHECK-LABEL: sil hidden @$s19function_conversion15convTupleScalar_2f22f3yyAA1Q_pc_yAaE_pcySi_SitSgctF
// CHECK:         function_ref @$s19function_conversion1Q_pIegn_AA1P_pIegn_TR
// CHECK:         function_ref @$s19function_conversion1Q_pIegn_AA1P_pIegn_TR
// CHECK:         function_ref @$sSi_SitSgIegy_S2iIegyy_TR

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s19function_conversion1Q_pIegn_AA1P_pIegn_TR : $@convention(thin) (@in_guaranteed P, @guaranteed @callee_guaranteed (@in_guaranteed Q) -> ()) -> ()

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sSi_SitSgIegy_S2iIegyy_TR : $@convention(thin) (Int, Int, @guaranteed @callee_guaranteed (Optional<(Int, Int)>) -> ()) -> ()

func convTupleScalar(_ f1: @escaping (Q) -> (),
                     f2: @escaping (_ parent: Q) -> (),
                     f3: @escaping (_ tuple: (Int, Int)?) -> ()) {
  let _: (P) -> () = f1
  let _: (P) -> () = f2
  let _: ((Int, Int)) -> () = f3
}

func convTupleScalarOpaque<T>(_ f: @escaping (T...) -> ()) -> ((_ args: T...) -> ())? {
  return f
}

// CHECK-LABEL: sil hidden @$s19function_conversion25convTupleToOptionalDirectySi_SitSgSicSi_SitSicF : $@convention(thin) (@guaranteed @callee_guaranteed (Int) -> (Int, Int)) -> @owned @callee_guaranteed (Int) -> Optional<(Int, Int)>
// CHECK:         bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed (Int) -> (Int, Int)):
// CHECK:           [[FN:%.*]] = copy_value [[ARG]]
// CHECK:           [[THUNK_FN:%.*]] = function_ref @$sS3iIegydd_S2i_SitSgIegyd_TR
// CHECK-NEXT:      [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]([[FN]])
// CHECK-NEXT:      return [[THUNK]]
// CHECK-NEXT: } // end sil function '$s19function_conversion25convTupleToOptionalDirectySi_SitSgSicSi_SitSicF'

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sS3iIegydd_S2i_SitSgIegyd_TR : $@convention(thin) (Int, @guaranteed @callee_guaranteed (Int) -> (Int, Int)) -> Optional<(Int, Int)>
// CHECK:         bb0(%0 : $Int, %1 : @guaranteed $@callee_guaranteed (Int) -> (Int, Int)):
// CHECK:           [[RESULT:%.*]] = apply %1(%0)
// CHECK-NEXT:      ([[LEFT:%.*]], [[RIGHT:%.*]]) = destructure_tuple [[RESULT]]
// CHECK-NEXT:      [[RESULT:%.*]] = tuple ([[LEFT]] : $Int, [[RIGHT]] : $Int)
// CHECK-NEXT:      [[OPTIONAL:%.*]] = enum $Optional<(Int, Int)>, #Optional.some!enumelt.1, [[RESULT]]
// CHECK-NEXT:      return [[OPTIONAL]]
// CHECK-NEXT: } // end sil function '$sS3iIegydd_S2i_SitSgIegyd_TR'

func convTupleToOptionalDirect(_ f: @escaping (Int) -> (Int, Int)) -> (Int) -> (Int, Int)? {
  return f
}

// CHECK-LABEL: sil hidden @$s19function_conversion27convTupleToOptionalIndirectyx_xtSgxcx_xtxclF : $@convention(thin) <T> (@guaranteed @callee_guaranteed (@in_guaranteed T) -> (@out T, @out T)) -> @owned @callee_guaranteed (@in_guaranteed T) -> @out Optional<(T, T)>
// CHECK:       bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed T) -> (@out T, @out T)):
// CHECK:          [[FN:%.*]] = copy_value [[ARG]]
// CHECK:          [[THUNK_FN:%.*]] = function_ref @$sxxxIegnrr_xx_xtSgIegnr_lTR
// CHECK-NEXT:     [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<T>([[FN]])
// CHECK-NEXT:     return [[THUNK]]
// CHECK-NEXT: } // end sil function '$s19function_conversion27convTupleToOptionalIndirectyx_xtSgxcx_xtxclF'

// CHECK:       sil shared [transparent] [serializable] [reabstraction_thunk] @$sxxxIegnrr_xx_xtSgIegnr_lTR : $@convention(thin) <T> (@in_guaranteed T, @guaranteed @callee_guaranteed (@in_guaranteed T) -> (@out T, @out T)) -> @out Optional<(T, T)>
// CHECK:       bb0(%0 : $*Optional<(T, T)>, %1 : $*T, %2 : @guaranteed $@callee_guaranteed (@in_guaranteed T) -> (@out T, @out T)):
// CHECK:         [[OPTIONAL:%.*]] = init_enum_data_addr %0 : $*Optional<(T, T)>, #Optional.some!enumelt.1
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[OPTIONAL]] : $*(T, T), 0
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[OPTIONAL]] : $*(T, T), 1
// CHECK-NEXT:    apply %2([[LEFT]], [[RIGHT]], %1)
// CHECK-NEXT:    inject_enum_addr %0 : $*Optional<(T, T)>, #Optional.some!enumelt.1
// CHECK-NEXT:    [[VOID:%.*]] = tuple ()
// CHECK:         return [[VOID]]

func convTupleToOptionalIndirect<T>(_ f: @escaping (T) -> (T, T)) -> (T) -> (T, T)? {
  return f
}

// ==== Make sure we support AnyHashable erasure

// CHECK-LABEL: sil hidden @$s19function_conversion15convAnyHashable1tyx_tSHRzlF
// CHECK:         function_ref @$s19function_conversion15convAnyHashable1tyx_tSHRzlFSbs0dE0V_AEtcfU_
// CHECK:         function_ref @$ss11AnyHashableVABSbIegnnd_xxSbIegnnd_SHRzlTR

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$ss11AnyHashableVABSbIegnnd_xxSbIegnnd_SHRzlTR : $@convention(thin) <T where T : Hashable> (@in_guaranteed T, @in_guaranteed T, @guaranteed @callee_guaranteed (@in_guaranteed AnyHashable, @in_guaranteed AnyHashable) -> Bool) -> Bool
// CHECK:         alloc_stack $AnyHashable
// CHECK:         function_ref @$ss21_convertToAnyHashableys0cD0VxSHRzlF
// CHECK:         apply {{.*}}<T>
// CHECK:         alloc_stack $AnyHashable
// CHECK:         function_ref @$ss21_convertToAnyHashableys0cD0VxSHRzlF
// CHECK:         apply {{.*}}<T>
// CHECK:         return

func convAnyHashable<T : Hashable>(t: T) {
  let fn: (T, T) -> Bool = {
    (x: AnyHashable, y: AnyHashable) in x == y
  }
}

// ==== Convert exploded tuples to Any or Optional<Any>

// CHECK-LABEL: sil hidden @$s19function_conversion12convTupleAnyyyyyc_Si_SitycyypcyypSgctF
// CHECK:         function_ref @$sIeg_ypIegr_TR
// CHECK:         partial_apply
// CHECK:         function_ref @$sIeg_ypSgIegr_TR
// CHECK:         partial_apply
// CHECK:         function_ref @$sS2iIegdd_ypIegr_TR
// CHECK:         partial_apply
// CHECK:         function_ref @$sS2iIegdd_ypSgIegr_TR
// CHECK:         partial_apply
// CHECK:         function_ref @$sypIegn_S2iIegyy_TR
// CHECK:         partial_apply
// CHECK:         function_ref @$sypSgIegn_S2iIegyy_TR
// CHECK:         partial_apply

func convTupleAny(_ f1: @escaping () -> (),
                  _ f2: @escaping () -> (Int, Int),
                  _ f3: @escaping (Any) -> (),
                  _ f4: @escaping (Any?) -> ()) {
  let _: () -> Any = f1
  let _: () -> Any? = f1

  let _: () -> Any = f2
  let _: () -> Any? = f2

  let _: ((Int, Int)) -> () = f3

  let _: ((Int, Int)) -> () = f4
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sIeg_ypIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @out Any
// CHECK:         init_existential_addr %0 : $*Any, $()
// CHECK-NEXT:    apply %1()
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sIeg_ypSgIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @out Optional<Any>
// CHECK:         [[ENUM_PAYLOAD:%.*]] = init_enum_data_addr %0 : $*Optional<Any>, #Optional.some!enumelt.1
// CHECK-NEXT:    init_existential_addr [[ENUM_PAYLOAD]] : $*Any, $()
// CHECK-NEXT:    apply %1()
// CHECK-NEXT:    inject_enum_addr %0 : $*Optional<Any>, #Optional.some!enumelt.1
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sS2iIegdd_ypIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> (Int, Int)) -> @out Any
// CHECK:         [[ANY_PAYLOAD:%.*]] = init_existential_addr %0
// CHECK-NEXT:    [[LEFT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    [[RIGHT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    [[RESULT:%.*]] = apply %1()
// CHECK-NEXT:    ([[LEFT:%.*]], [[RIGHT:%.*]]) = destructure_tuple [[RESULT]]
// CHECK-NEXT:    store [[LEFT:%.*]] to [trivial] [[LEFT_ADDR]]
// CHECK-NEXT:    store [[RIGHT:%.*]] to [trivial] [[RIGHT_ADDR]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sS2iIegdd_ypSgIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> (Int, Int)) -> @out Optional<Any> {
// CHECK:         [[OPTIONAL_PAYLOAD:%.*]] = init_enum_data_addr %0
// CHECK-NEXT:    [[ANY_PAYLOAD:%.*]] = init_existential_addr [[OPTIONAL_PAYLOAD]]
// CHECK-NEXT:    [[LEFT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    [[RIGHT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    [[RESULT:%.*]] = apply %1()
// CHECK-NEXT:    ([[LEFT:%.*]], [[RIGHT:%.*]]) = destructure_tuple [[RESULT]]
// CHECK-NEXT:    store [[LEFT:%.*]] to [trivial] [[LEFT_ADDR]]
// CHECK-NEXT:    store [[RIGHT:%.*]] to [trivial] [[RIGHT_ADDR]]
// CHECK-NEXT:    inject_enum_addr %0
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypIegn_S2iIegyy_TR : $@convention(thin) (Int, Int, @guaranteed @callee_guaranteed (@in_guaranteed Any) -> ()) -> ()
// CHECK:         [[ANY_VALUE:%.*]] = alloc_stack $Any
// CHECK-NEXT:    [[ANY_PAYLOAD:%.*]] = init_existential_addr [[ANY_VALUE]]
// CHECK-NEXT:    [[LEFT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    store %0 to [trivial] [[LEFT_ADDR]]
// CHECK-NEXT:    [[RIGHT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    store %1 to [trivial] [[RIGHT_ADDR]]
// CHECK-NEXT:    apply %2([[ANY_VALUE]])
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    destroy_addr [[ANY_VALUE]]
// CHECK-NEXT:    dealloc_stack [[ANY_VALUE]]
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypSgIegn_S2iIegyy_TR : $@convention(thin) (Int, Int, @guaranteed @callee_guaranteed (@in_guaranteed Optional<Any>) -> ()) -> ()
// CHECK:         [[ANY_VALUE:%.*]] = alloc_stack $Any
// CHECK-NEXT:    [[ANY_PAYLOAD:%.*]] = init_existential_addr [[ANY_VALUE]]
// CHECK-NEXT:    [[LEFT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    store %0 to [trivial] [[LEFT_ADDR]]
// CHECK-NEXT:    [[RIGHT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    store %1 to [trivial] [[RIGHT_ADDR]]
// CHECK-NEXT:    [[OPTIONAL_VALUE:%.*]] = alloc_stack $Optional<Any>
// CHECK-NEXT:    [[OPTIONAL_PAYLOAD:%.*]] = init_enum_data_addr [[OPTIONAL_VALUE]]
// CHECK-NEXT:    copy_addr [take] [[ANY_VALUE]] to [initialization] [[OPTIONAL_PAYLOAD]]
// CHECK-NEXT:    inject_enum_addr [[OPTIONAL_VALUE]]
// CHECK-NEXT:    apply %2([[OPTIONAL_VALUE]])
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    destroy_addr [[OPTIONAL_VALUE]]
// CHECK-NEXT:    dealloc_stack [[OPTIONAL_VALUE]]
// CHECK-NEXT:    dealloc_stack [[ANY_VALUE]]
// CHECK-NEXT:    return

// ==== Support collection subtyping in function argument position

protocol Z {}
class A: Z {}

func foo_arr<T: Z>(type: T.Type, _ fn: ([T]?) -> Void) {}
func foo_map<T: Z>(type: T.Type, _ fn: ([Int: T]) -> Void) {}

func rdar35702810() {
  let fn_arr: ([Z]?) -> Void = { _ in }
  let fn_map: ([Int: Z]) -> Void = { _ in }

  // CHECK: function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  // CHECK: apply %5<A, Z>(%4) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
  foo_arr(type: A.self, fn_arr)

  // CHECK: function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: apply %2<Int, A, Int, Z>(%0) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
  // CHECK: apply %1(%4) : $@callee_guaranteed (@guaranteed Dictionary<Int, Z>) -> ()
  foo_map(type: A.self, fn_map)
}

protocol X: Hashable {}
class B: X {
  var hashValue: Int { return 42 }
  static func == (lhs: B, rhs: B) -> Bool {
    return lhs.hashValue == rhs.hashValue
  }
}

func bar_arr<T: X>(type: T.Type, _ fn: ([T]?) -> Void) {}
func bar_map<T: X>(type: T.Type, _ fn: ([T: Int]) -> Void) {}
func bar_set<T: X>(type: T.Type, _ fn: (Set<T>) -> Void) {}

func rdar35702810_anyhashable() {
  let fn_arr: ([AnyHashable]?) -> Void = { _ in }
  let fn_map: ([AnyHashable: Int]) -> Void = { _ in }
  let fn_set: (Set<AnyHashable>) -> Void = { _ in }


  // CHECK: [[FN:%.*]] = function_ref @$sSays11AnyHashableVGSgIegg_Say19function_conversion1BCGSgIegg_TR : $@convention(thin) (@guaranteed Optional<Array<B>>, @guaranteed @callee_guaranteed (@guaranteed Optional<Array<AnyHashable>>) -> ()) -> ()
  // CHECK: [[PA:%.*]] = partial_apply [callee_guaranteed] [[FN]](%{{[0-9]+}}) : $@convention(thin) (@guaranteed Optional<Array<B>>, @guaranteed @callee_guaranteed (@guaranteed Optional<Array<AnyHashable>>) -> ()) -> ()
  // CHECK: convert_escape_to_noescape [not_guaranteed] [[PA]] : $@callee_guaranteed (@guaranteed Optional<Array<B>>) -> () to $@noescape @callee_guaranteed (@guaranteed Optional<Array<B>>) -> ()
  bar_arr(type: B.self, fn_arr)

  // CHECK: [[FN:%.*]] = function_ref @$sSDys11AnyHashableVSiGIegg_SDy19function_conversion1BCSiGIegg_TR : $@convention(thin) (@guaranteed Dictionary<B, Int>, @guaranteed @callee_guaranteed (@guaranteed Dictionary<AnyHashable, Int>) -> ()) -> ()
  // CHECK: [[PA:%.*]] = partial_apply [callee_guaranteed] [[FN]](%{{[0-9]+}}) : $@convention(thin) (@guaranteed Dictionary<B, Int>, @guaranteed @callee_guaranteed (@guaranteed Dictionary<AnyHashable, Int>) -> ()) -> ()
  // CHECK: convert_escape_to_noescape [not_guaranteed] [[PA]] : $@callee_guaranteed (@guaranteed Dictionary<B, Int>) -> () to $@noescape @callee_guaranteed (@guaranteed Dictionary<B, Int>) -> ()
  bar_map(type: B.self, fn_map)

  // CHECK: [[FN:%.*]] = function_ref @$sShys11AnyHashableVGIegg_Shy19function_conversion1BCGIegg_TR : $@convention(thin) (@guaranteed Set<B>, @guaranteed @callee_guaranteed (@guaranteed Set<AnyHashable>) -> ()) -> ()
  // CHECK: [[PA:%.*]] = partial_apply [callee_guaranteed] [[FN]](%{{[0-9]+}}) : $@convention(thin) (@guaranteed Set<B>, @guaranteed @callee_guaranteed (@guaranteed Set<AnyHashable>) -> ()) -> ()
  // CHECK: convert_escape_to_noescape [not_guaranteed] [[PA]] : $@callee_guaranteed (@guaranteed Set<B>) -> () to $@noescape @callee_guaranteed (@guaranteed Set<B>) -> ()
  bar_set(type: B.self, fn_set)
}

// ==== Function conversion with parameter substToOrig reabstraction.

struct FunctionConversionParameterSubstToOrigReabstractionTest {
  typealias SelfTy = FunctionConversionParameterSubstToOrigReabstractionTest

  class Klass: Error {}

  struct Foo<T> {
    static func enum1Func(_ : (T) -> Foo<Error>) -> Foo<Error> {
      // Just to make it compile.
      return Optional<Foo<Error>>.none!
    }
  }

  static func bar<T>(t: T) -> Foo<T> {
    // Just to make it compile.
    return Optional<Foo<T>>.none!
  }

  static func testFunc() -> Foo<Error> {
    return Foo<Klass>.enum1Func(SelfTy.bar)
  }
}

// CHECK: sil {{.*}} @$sS4SIgggoo_S2Ss11AnyHashableVyps5Error_pIegggrrzo_TR
// CHECK:  [[TUPLE:%.*]] = apply %4(%2, %3) : $@noescape @callee_guaranteed (@guaranteed String, @guaranteed String) -> (@owned String, @owned String)
// CHECK:  ([[LHS:%.*]], [[RHS:%.*]]) = destructure_tuple [[TUPLE]]
// CHECK:  [[ADDR:%.*]] = alloc_stack $String
// CHECK:  store [[LHS]] to [init] [[ADDR]] : $*String
// CHECK:  [[CVT:%.*]] = function_ref @$ss21_convertToAnyHashableys0cD0VxSHRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : Hashable> (@in_guaranteed τ_0_0) -> @out AnyHashable
// CHECK:  apply [[CVT]]<String>(%0, [[ADDR]])
// CHECK: } // end sil function '$sS4SIgggoo_S2Ss11AnyHashableVyps5Error_pIegggrrzo_TR'

func dontCrash() {
  let userInfo = ["hello": "world"]
  let d = [AnyHashable: Any](uniqueKeysWithValues: userInfo.map { ($0.key, $0.value) })
}
