// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// Check SILGen against various FunctionConversionExprs emitted by Sema.

// ==== Representation conversions

// CHECK-LABEL: sil hidden @_T019function_conversion7cToFuncS2icS2iXCF : $@convention(thin) (@convention(c) (Int) -> Int) -> @owned @callee_owned (Int) -> Int
// CHECK:         [[THUNK:%.*]] = function_ref @_T0S2iItCyd_S2iIxyd_TR
// CHECK:         [[FUNC:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:         return [[FUNC]]
func cToFunc(_ arg: @escaping @convention(c) (Int) -> Int) -> (Int) -> Int {
  return arg
}

// CHECK-LABEL: sil hidden @_T019function_conversion8cToBlockS2iXBS2iXCF : $@convention(thin) (@convention(c) (Int) -> Int) -> @owned @convention(block) (Int) -> Int
// CHECK:         [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// CHECK:         [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]]
// CHECK:         [[COPY:%.*]] = copy_block [[BLOCK]] : $@convention(block) (Int) -> Int
// CHECK:         return [[COPY]]
func cToBlock(_ arg: @escaping @convention(c) (Int) -> Int) -> @convention(block) (Int) -> Int {
  return arg
}

// ==== Throws variance

// CHECK-LABEL: sil hidden @_T019function_conversion12funcToThrowsyyKcyycF : $@convention(thin) (@owned @callee_owned () -> ()) -> @owned @callee_owned () -> @error Error
// CHECK: bb0([[ARG:%.*]] : $@callee_owned () -> ()):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:   [[FUNC:%.*]] = convert_function [[ARG_COPY]] : $@callee_owned () -> () to $@callee_owned () -> @error Error
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK:   return [[FUNC]]
// CHECK: } // end sil function '_T019function_conversion12funcToThrowsyyKcyycF'
func funcToThrows(_ x: @escaping () -> ()) -> () throws -> () {
  return x
}

// CHECK-LABEL: sil hidden @_T019function_conversion12thinToThrowsyyKXfyyXfF : $@convention(thin) (@convention(thin) () -> ()) -> @convention(thin) () -> @error Error
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

// CHECK-LABEL: sil hidden @_T019function_conversion12funcToUpcastAA5FeralCycAA12DomesticatedCycF : $@convention(thin) (@owned @callee_owned () -> @owned Domesticated) -> @owned @callee_owned () -> @owned Feral {
// CHECK: bb0([[ARG:%.*]] : $@callee_owned () -> @owned Domesticated):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:   [[FUNC:%.*]] = convert_function [[ARG_COPY]] : $@callee_owned () -> @owned Domesticated to $@callee_owned () -> @owned Feral
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK:   return [[FUNC]]
// CHECK: } // end sil function '_T019function_conversion12funcToUpcastAA5FeralCycAA12DomesticatedCycF'
func funcToUpcast(_ x: @escaping () -> Domesticated) -> () -> Feral {
  return x
}

// CHECK-LABEL: sil hidden @_T019function_conversion12funcToUpcastyAA12DomesticatedCcyAA5FeralCcF : $@convention(thin) (@owned @callee_owned (@owned Feral) -> ()) -> @owned @callee_owned (@owned Domesticated) -> ()
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:   [[FUNC:%.*]] = convert_function [[ARG_COPY]] : $@callee_owned (@owned Feral) -> () to $@callee_owned (@owned Domesticated) -> (){{.*}}
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
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

// CHECK-LABEL: sil hidden @_T019function_conversion19convOptionalTrivialyAA0E0VADSgcF
func convOptionalTrivial(_ t1: @escaping (Trivial?) -> Trivial) {
// CHECK:         function_ref @_T019function_conversion7TrivialVSgACIxyd_AcDIxyd_TR
// CHECK:         partial_apply
  let _: (Trivial) -> Trivial? = t1

// CHECK:         function_ref @_T019function_conversion7TrivialVSgACIxyd_A2DIxyd_TR
// CHECK:         partial_apply
  let _: (Trivial!) -> Trivial? = t1
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion7TrivialVSgACIxyd_AcDIxyd_TR : $@convention(thin) (Trivial, @owned @callee_owned (Optional<Trivial>) -> Trivial) -> Optional<Trivial>
// CHECK:         enum $Optional<Trivial>
// CHECK-NEXT:    apply %1(%2)
// CHECK-NEXT:    enum $Optional<Trivial>
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion7TrivialVSgACIxyd_A2DIxyd_TR : $@convention(thin) (Optional<Trivial>, @owned @callee_owned (Optional<Trivial>) -> Trivial) -> Optional<Trivial>
// CHECK:         apply %1(%0)
// CHECK-NEXT:    enum $Optional<Trivial>
// CHECK-NEXT:    return

// CHECK-LABEL: sil hidden @_T019function_conversion20convOptionalLoadableyAA0E0VADSgcF
func convOptionalLoadable(_ l1: @escaping (Loadable?) -> Loadable) {
// CHECK:         function_ref @_T019function_conversion8LoadableVSgACIxxo_AcDIxxo_TR
// CHECK:         partial_apply
  let _: (Loadable) -> Loadable? = l1

// CHECK:         function_ref @_T019function_conversion8LoadableVSgACIxxo_A2DIxxo_TR
// CHECK:         partial_apply
  let _: (Loadable!) -> Loadable? = l1
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion8LoadableVSgACIxxo_A2DIxxo_TR : $@convention(thin) (@owned Optional<Loadable>, @owned @callee_owned (@owned Optional<Loadable>) -> @owned Loadable) -> @owned Optional<Loadable>
// CHECK:         apply %1(%0)
// CHECK-NEXT:    enum $Optional<Loadable>
// CHECK-NEXT:    return

// CHECK-LABEL: sil hidden @_T019function_conversion20convOptionalAddrOnlyyAA0eF0VADSgcF
func convOptionalAddrOnly(_ a1: @escaping (AddrOnly?) -> AddrOnly) {
// CHECK:         function_ref @_T019function_conversion8AddrOnlyVSgACIxir_A2DIxir_TR
// CHECK:         partial_apply
  let _: (AddrOnly?) -> AddrOnly? = a1

// CHECK:         function_ref @_T019function_conversion8AddrOnlyVSgACIxir_A2DIxir_TR
// CHECK:         partial_apply
  let _: (AddrOnly!) -> AddrOnly? = a1
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion8AddrOnlyVSgACIxir_A2DIxir_TR : $@convention(thin) (@in Optional<AddrOnly>, @owned @callee_owned (@in Optional<AddrOnly>) -> @out AddrOnly) -> @out Optional<AddrOnly>
// CHECK:         alloc_stack $AddrOnly
// CHECK-NEXT:    apply %2(%3, %1)
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

// CHECK-LABEL: sil hidden @_T019function_conversion22convExistentialTrivialyAA0E0VAA1Q_pc_AdaE_pSgc2t3tF
func convExistentialTrivial(_ t2: @escaping (Q) -> Trivial, t3: @escaping (Q?) -> Trivial) {
// CHECK:         function_ref @_T019function_conversion1Q_pAA7TrivialVIxid_AdA1P_pIxyr_TR
// CHECK:         partial_apply
  let _: (Trivial) -> P = t2

// CHECK:         function_ref @_T019function_conversion1Q_pSgAA7TrivialVIxid_AESgAA1P_pIxyr_TR
// CHECK:         partial_apply
  let _: (Trivial?) -> P = t3

// CHECK:         function_ref @_T019function_conversion1Q_pAA7TrivialVIxid_AA1P_pAaE_pIxir_TR
// CHECK:         partial_apply
  let _: (P) -> P = t2
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion1Q_pAA7TrivialVIxid_AdA1P_pIxyr_TR : $@convention(thin) (Trivial, @owned @callee_owned (@in Q) -> Trivial) -> @out P
// CHECK:         alloc_stack $Q
// CHECK-NEXT:    init_existential_addr
// CHECK-NEXT:    store
// CHECK-NEXT:    apply
// CHECK-NEXT:    init_existential_addr
// CHECK-NEXT:    store
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion1Q_pSgAA7TrivialVIxid_AESgAA1P_pIxyr_TR
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

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion1Q_pAA7TrivialVIxid_AA1P_pAaE_pIxir_TR : $@convention(thin) (@in P, @owned @callee_owned (@in Q) -> Trivial) -> @out P
// CHECK:         alloc_stack $Q
// CHECK-NEXT:    open_existential_addr immutable_access %1 : $*P
// CHECK-NEXT:    init_existential_addr %3 : $*Q
// CHECK-NEXT:    copy_addr [take] {{.*}} to [initialization] {{.*}}
// CHECK-NEXT:    apply
// CHECK-NEXT:    init_existential_addr
// CHECK-NEXT:    store
// CHECK:         deinit_existential_addr
// CHECK:         return

// ==== Existential metatypes

// CHECK-LABEL: sil hidden @_T019function_conversion23convExistentialMetatypeyAA7TrivialVmAA1Q_pXpSgcF
func convExistentialMetatype(_ em: @escaping (Q.Type?) -> Trivial.Type) {
// CHECK:         function_ref @_T019function_conversion1Q_pXmTSgAA7TrivialVXMtIxyd_AEXMtAA1P_pXmTIxyd_TR
// CHECK:         partial_apply
  let _: (Trivial.Type) -> P.Type = em

// CHECK:         function_ref @_T019function_conversion1Q_pXmTSgAA7TrivialVXMtIxyd_AEXMtSgAA1P_pXmTIxyd_TR
// CHECK:         partial_apply
  let _: (Trivial.Type?) -> P.Type = em

// CHECK:         function_ref @_T019function_conversion1Q_pXmTSgAA7TrivialVXMtIxyd_AA1P_pXmTAaF_pXmTIxyd_TR
// CHECK:         partial_apply
  let _: (P.Type) -> P.Type = em
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion1Q_pXmTSgAA7TrivialVXMtIxyd_AEXMtAA1P_pXmTIxyd_TR : $@convention(thin) (@thin Trivial.Type, @owned @callee_owned (Optional<@thick Q.Type>) -> @thin Trivial.Type) -> @thick P.Type
// CHECK:         metatype $@thick Trivial.Type
// CHECK-NEXT:    init_existential_metatype %2 : $@thick Trivial.Type, $@thick Q.Type
// CHECK-NEXT:    enum $Optional<@thick Q.Type>
// CHECK-NEXT:    apply
// CHECK-NEXT:    metatype $@thick Trivial.Type
// CHECK-NEXT:    init_existential_metatype {{.*}} : $@thick Trivial.Type, $@thick P.Type
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion1Q_pXmTSgAA7TrivialVXMtIxyd_AEXMtSgAA1P_pXmTIxyd_TR : $@convention(thin) (Optional<@thin Trivial.Type>, @owned @callee_owned (Optional<@thick Q.Type>) -> @thin Trivial.Type) -> @thick P.Type
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

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion1Q_pXmTSgAA7TrivialVXMtIxyd_AA1P_pXmTAaF_pXmTIxyd_TR : $@convention(thin) (@thick P.Type, @owned @callee_owned (Optional<@thick Q.Type>) -> @thin Trivial.Type) -> @thick P.Type
// CHECK:         open_existential_metatype %0 : $@thick P.Type to $@thick (@opened({{.*}}) P).Type
// CHECK-NEXT:    init_existential_metatype %2 : $@thick (@opened({{.*}}) P).Type, $@thick Q.Type
// CHECK-NEXT:    enum $Optional<@thick Q.Type>
// CHECK-NEXT:    apply
// CHECK-NEXT:    metatype $@thick Trivial.Type
// CHECK-NEXT:    init_existential_metatype {{.*}} : $@thick Trivial.Type, $@thick P.Type
// CHECK-NEXT:    return

// ==== Class metatype upcasts

class Parent {}
class Child : Parent {}

// Note: we add a Trivial => Trivial? conversion here to force a thunk
// to be generated

// CHECK-LABEL: sil hidden @_T019function_conversion18convUpcastMetatypeyAA5ChildCmAA6ParentCm_AA7TrivialVSgtc_ADmAFmSg_AItc2c5tF
func convUpcastMetatype(_ c4: @escaping (Parent.Type, Trivial?) -> Child.Type,
                        c5: @escaping (Parent.Type?, Trivial?) -> Child.Type) {
// CHECK:         function_ref @_T019function_conversion6ParentCXMTAA7TrivialVSgAA5ChildCXMTIxyyd_AHXMTAeCXMTIxyyd_TR
// CHECK:         partial_apply
  let _: (Child.Type, Trivial) -> Parent.Type = c4

// CHECK:         function_ref @_T019function_conversion6ParentCXMTSgAA7TrivialVSgAA5ChildCXMTIxyyd_AIXMTAfCXMTIxyyd_TR
// CHECK:         partial_apply
  let _: (Child.Type, Trivial) -> Parent.Type = c5

// CHECK:         function_ref @_T019function_conversion6ParentCXMTSgAA7TrivialVSgAA5ChildCXMTIxyyd_AIXMTSgAfDIxyyd_TR
// CHECK:         partial_apply
  let _: (Child.Type?, Trivial) -> Parent.Type? = c5
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion6ParentCXMTAA7TrivialVSgAA5ChildCXMTIxyyd_AHXMTAeCXMTIxyyd_TR : $@convention(thin) (@thick Child.Type, Trivial, @owned @callee_owned (@thick Parent.Type, Optional<Trivial>) -> @thick Child.Type) -> @thick Parent.Type
// CHECK:         upcast %0 : $@thick Child.Type to $@thick Parent.Type
// CHECK:         apply
// CHECK:         upcast {{.*}} : $@thick Child.Type to $@thick Parent.Type
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion6ParentCXMTSgAA7TrivialVSgAA5ChildCXMTIxyyd_AIXMTAfCXMTIxyyd_TR : $@convention(thin) (@thick Child.Type, Trivial, @owned @callee_owned (Optional<@thick Parent.Type>, Optional<Trivial>) -> @thick Child.Type) -> @thick Parent.Type
// CHECK:         upcast %0 : $@thick Child.Type to $@thick Parent.Type
// CHECK:         enum $Optional<@thick Parent.Type>
// CHECK:         apply
// CHECK:         upcast {{.*}} : $@thick Child.Type to $@thick Parent.Type
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion6ParentCXMTSgAA7TrivialVSgAA5ChildCXMTIxyyd_AIXMTSgAfDIxyyd_TR : $@convention(thin) (Optional<@thick Child.Type>, Trivial, @owned @callee_owned (Optional<@thick Parent.Type>, Optional<Trivial>) -> @thick Child.Type) -> Optional<@thick Parent.Type>
// CHECK:         unchecked_trivial_bit_cast %0 : $Optional<@thick Child.Type> to $Optional<@thick Parent.Type>
// CHECK:         apply
// CHECK:         upcast {{.*}} : $@thick Child.Type to $@thick Parent.Type
// CHECK:         enum $Optional<@thick Parent.Type>
// CHECK:         return

// ==== Function to existential -- make sure we maximally abstract it

// CHECK-LABEL: sil hidden @_T019function_conversion19convFuncExistentialyS2icypcF : $@convention(thin) (@owned @callee_owned (@in Any) -> @owned @callee_owned (Int) -> Int) -> ()
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:   [[REABSTRACT_THUNK:%.*]] = function_ref @_T0ypS2iIxyd_Ixio_S2iIxyd_ypIxxr_TR
// CHECK:   [[PA:%.*]] = partial_apply [[REABSTRACT_THUNK]]([[ARG_COPY]])
// CHECK:   destroy_value [[PA]]
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK: } // end sil function '_T019function_conversion19convFuncExistentialyS2icypcF'
func convFuncExistential(_ f1: @escaping (Any) -> (Int) -> Int) {
  let _: ((Int) -> Int) -> Any = f1
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0ypS2iIxyd_Ixio_S2iIxyd_ypIxxr_TR : $@convention(thin) (@owned @callee_owned (Int) -> Int, @owned @callee_owned (@in Any) -> @owned @callee_owned (Int) -> Int) -> @out Any
// CHECK:         alloc_stack $Any
// CHECK:         function_ref @_T0S2iIxyd_S2iIxir_TR
// CHECK-NEXT:    partial_apply
// CHECK-NEXT:    init_existential_addr %3 : $*Any, $(Int) -> Int
// CHECK-NEXT:    store
// CHECK-NEXT:    apply
// CHECK:         function_ref @_T0S2iIxyd_S2iIxir_TR
// CHECK-NEXT:    partial_apply
// CHECK-NEXT:    init_existential_addr %0 : $*Any, $(Int) -> Int
// CHECK-NEXT:    store {{.*}} to {{.*}} : $*@callee_owned (@in Int) -> @out Int
// CHECK:         return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0S2iIxyd_S2iIxir_TR : $@convention(thin) (@in Int, @owned @callee_owned (Int) -> Int) -> @out Int
// CHECK:         load [trivial] %1 : $*Int
// CHECK-NEXT:    apply %2(%3)
// CHECK-NEXT:    store {{.*}} to [trivial] %0
// CHECK:         return

// ==== Class-bound archetype upcast

// CHECK-LABEL: sil hidden @_T019function_conversion29convClassBoundArchetypeUpcast{{[_0-9a-zA-Z]*}}F
func convClassBoundArchetypeUpcast<T : Parent>(_ f1: @escaping (Parent) -> (T, Trivial)) {
// CHECK:         function_ref @_T019function_conversion6ParentCxAA7TrivialVIxxod_xAcESgIxxod_ACRbzlTR
// CHECK:         partial_apply
  let _: (T) -> (Parent, Trivial?) = f1
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion6ParentCxAA7TrivialVIxxod_xAcESgIxxod_ACRbzlTR : $@convention(thin) <T where T : Parent> (@owned T, @owned @callee_owned (@owned Parent) -> (@owned T, Trivial)) -> (@owned Parent, Optional<Trivial>)
// CHECK: bb0([[ARG:%.*]] : $T, [[CLOSURE:%.*]] : $@callee_owned (@owned Parent) -> (@owned T, Trivial)):
// CHECK:    [[CASTED_ARG:%.*]] = upcast [[ARG]] : $T to $Parent
// CHECK:    [[RESULT:%.*]] = apply [[CLOSURE]]([[CASTED_ARG]])
// CHECK:    [[BORROWED_RESULT:%.*]] = begin_borrow [[RESULT]] : $(T, Trivial)
// CHECK:    [[FIRST_RESULT:%.*]] = tuple_extract [[BORROWED_RESULT]] : $(T, Trivial), 0
// CHECK:    [[COPIED_FIRST_RESULT:%.*]] = copy_value [[FIRST_RESULT]]
// CHECK:    tuple_extract [[BORROWED_RESULT]] : $(T, Trivial), 1
// CHECK:    end_borrow [[BORROWED_RESULT]] from [[RESULT]]
// CHECK:    destroy_value [[RESULT]]
// CHECK:    [[CAST_COPIED_FIRST_RESULT:%.*]] = upcast [[COPIED_FIRST_RESULT]] : $T to $Parent
// CHECK:    enum $Optional<Trivial>
// CHECK:    [[RESULT:%.*]] = tuple ([[CAST_COPIED_FIRST_RESULT]] : $Parent, {{.*}} : $Optional<Trivial>)
// CHECK:    return [[RESULT]]

// CHECK-LABEL: sil hidden @_T019function_conversion37convClassBoundMetatypeArchetypeUpcast{{[_0-9a-zA-Z]*}}F
func convClassBoundMetatypeArchetypeUpcast<T : Parent>(_ f1: @escaping (Parent.Type) -> (T.Type, Trivial)) {
// CHECK:         function_ref @_T019function_conversion6ParentCXMTxXMTAA7TrivialVIxydd_xXMTACXMTAESgIxydd_ACRbzlTR
// CHECK:         partial_apply
  let _: (T.Type) -> (Parent.Type, Trivial?) = f1
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion6ParentCXMTxXMTAA7TrivialVIxydd_xXMTACXMTAESgIxydd_ACRbzlTR : $@convention(thin) <T where T : Parent> (@thick T.Type, @owned @callee_owned (@thick Parent.Type) -> (@thick T.Type, Trivial)) -> (@thick Parent.Type, Optional<Trivial>)
// CHECK:         upcast %0 : $@thick T.Type to $@thick Parent.Type
// CHECK-NEXT:    apply
// CHECK-NEXT:    tuple_extract
// CHECK-NEXT:    tuple_extract
// CHECK-NEXT:    upcast {{.*}} : $@thick T.Type to $@thick Parent.Type
// CHECK-NEXT:    enum $Optional<Trivial>
// CHECK-NEXT:    tuple
// CHECK-NEXT:    return

// ==== Make sure we destructure one-element tuples

// CHECK-LABEL: sil hidden @_T019function_conversion15convTupleScalaryyAA1Q_pc_yAaC_pc2f2ySi_SitSgc2f3tF
// CHECK:         function_ref @_T019function_conversion1Q_pIxi_AA1P_pIxi_TR
// CHECK:         function_ref @_T019function_conversion1Q_pIxi_AA1P_pIxi_TR
// CHECK:         function_ref @_T0Si_SitSgIxy_S2iIxyy_TR

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T019function_conversion1Q_pIxi_AA1P_pIxi_TR : $@convention(thin) (@in P, @owned @callee_owned (@in Q) -> ()) -> ()

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0Si_SitSgIxy_S2iIxyy_TR : $@convention(thin) (Int, Int, @owned @callee_owned (Optional<(Int, Int)>) -> ()) -> ()

func convTupleScalar(_ f1: @escaping (Q) -> (),
                     f2: @escaping (_ parent: Q) -> (),
                     f3: @escaping (_ tuple: (Int, Int)?) -> ()) {
  let _: (P) -> () = f1
  let _: (P) -> () = f2
  let _: (Int, Int) -> () = f3
}

func convTupleScalarOpaque<T>(_ f: @escaping (T...) -> ()) -> ((_ args: T...) -> ())? {
  return f
}

// CHECK-LABEL: sil hidden @_T019function_conversion25convTupleToOptionalDirectSi_SitSgSicSi_SitSicF : $@convention(thin) (@owned @callee_owned (Int) -> (Int, Int)) -> @owned @callee_owned (Int) -> Optional<(Int, Int)>
// CHECK:         bb0([[ARG:%.*]] : $@callee_owned (Int) -> (Int, Int)):
// CHECK:           [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:           [[FN:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:           [[THUNK_FN:%.*]] = function_ref @_T0S3iIxydd_S2i_SitSgIxyd_TR
// CHECK-NEXT:      [[THUNK:%.*]] = partial_apply [[THUNK_FN]]([[FN]])
// CHECK-NEXT:      end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT:      destroy_value [[ARG]]
// CHECK-NEXT:      return [[THUNK]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0S3iIxydd_S2i_SitSgIxyd_TR : $@convention(thin) (Int, @owned @callee_owned (Int) -> (Int, Int)) -> Optional<(Int, Int)>
// CHECK:         bb0(%0 : $Int, %1 : $@callee_owned (Int) -> (Int, Int)):
// CHECK:           [[RESULT:%.*]] = apply %1(%0)
// CHECK-NEXT:      [[LEFT:%.*]] = tuple_extract [[RESULT]]
// CHECK-NEXT:      [[RIGHT:%.*]] = tuple_extract [[RESULT]]
// CHECK-NEXT:      [[RESULT:%.*]] = tuple ([[LEFT]] : $Int, [[RIGHT]] : $Int)
// CHECK-NEXT:      [[OPTIONAL:%.*]] = enum $Optional<(Int, Int)>, #Optional.some!enumelt.1, [[RESULT]]
// CHECK-NEXT:      return [[OPTIONAL]]

func convTupleToOptionalDirect(_ f: @escaping (Int) -> (Int, Int)) -> (Int) -> (Int, Int)? {
  return f
}

// CHECK-LABEL: sil hidden @_T019function_conversion27convTupleToOptionalIndirectx_xtSgxcx_xtxclF : $@convention(thin) <T> (@owned @callee_owned (@in T) -> (@out T, @out T)) -> @owned @callee_owned (@in T) -> @out Optional<(T, T)>
// CHECK:       bb0([[ARG:%.*]] : $@callee_owned (@in T) -> (@out T, @out T)):
// CHECK:          [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:          [[FN:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:          [[THUNK_FN:%.*]] = function_ref @_T0xxxIxirr_xx_xtSgIxir_lTR
// CHECK-NEXT:     [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<T>([[FN]])
// CHECK-NEXT:     end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT:     destroy_value [[ARG]]
// CHECK-NEXT:     return [[THUNK]]

// CHECK:       sil shared [transparent] [reabstraction_thunk] @_T0xxxIxirr_xx_xtSgIxir_lTR : $@convention(thin) <T> (@in T, @owned @callee_owned (@in T) -> (@out T, @out T)) -> @out Optional<(T, T)>
// CHECK:       bb0(%0 : $*Optional<(T, T)>, %1 : $*T, %2 : $@callee_owned (@in T) -> (@out T, @out T)):
// CHECK:         [[OPTIONAL:%.*]] = init_enum_data_addr %0 : $*Optional<(T, T)>, #Optional.some!enumelt.1
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr %3 : $*(T, T), 0
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr %3 : $*(T, T), 1
// CHECK-NEXT:    apply %2([[LEFT]], [[RIGHT]], %1)
// CHECK-NEXT:    inject_enum_addr %0 : $*Optional<(T, T)>, #Optional.some!enumelt.1
// CHECK:         return

func convTupleToOptionalIndirect<T>(_ f: @escaping (T) -> (T, T)) -> (T) -> (T, T)? {
  return f
}

// ==== Make sure we support AnyHashable erasure

// CHECK-LABEL: sil hidden @_T019function_conversion15convAnyHashableyx1t_ts0E0RzlF
// CHECK:         function_ref @_T019function_conversion15convAnyHashableyx1t_ts0E0RzlFSbs0dE0V_AFtcfU_
// CHECK:         function_ref @_T0s11AnyHashableVABSbIxiid_xxSbIxiid_s0B0RzlTR

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0s11AnyHashableVABSbIxiid_xxSbIxiid_s0B0RzlTR : $@convention(thin) <T where T : Hashable> (@in T, @in T, @owned @callee_owned (@in AnyHashable, @in AnyHashable) -> Bool) -> Bool
// CHECK:         alloc_stack $AnyHashable
// CHECK:         function_ref @_swift_convertToAnyHashable
// CHECK:         apply {{.*}}<T>
// CHECK:         alloc_stack $AnyHashable
// CHECK:         function_ref @_swift_convertToAnyHashable
// CHECK:         apply {{.*}}<T>
// CHECK:         return

func convAnyHashable<T : Hashable>(t: T) {
  let fn: (T, T) -> Bool = {
    (x: AnyHashable, y: AnyHashable) in x == y
  }
}

// ==== Convert exploded tuples to Any or Optional<Any>

// CHECK-LABEL: sil hidden @_T019function_conversion12convTupleAnyyyyc_Si_SitycyypcyypSgctF
// CHECK:         function_ref @_T0Ix_ypIxr_TR
// CHECK:         partial_apply
// CHECK:         function_ref @_T0Ix_ypSgIxr_TR
// CHECK:         partial_apply
// CHECK:         function_ref @_T0S2iIxdd_ypIxr_TR
// CHECK:         partial_apply
// CHECK:         function_ref @_T0S2iIxdd_ypSgIxr_TR
// CHECK:         partial_apply
// CHECK:         function_ref @_T0ypIxi_S2iIxyy_TR
// CHECK:         partial_apply
// CHECK:         function_ref @_T0ypSgIxi_S2iIxyy_TR
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

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0Ix_ypIxr_TR : $@convention(thin) (@owned @callee_owned () -> ()) -> @out Any
// CHECK:         init_existential_addr %0 : $*Any, $()
// CHECK-NEXT:    apply %1()
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0Ix_ypSgIxr_TR : $@convention(thin) (@owned @callee_owned () -> ()) -> @out Optional<Any>
// CHECK:         [[ENUM_PAYLOAD:%.*]] = init_enum_data_addr %0 : $*Optional<Any>, #Optional.some!enumelt.1
// CHECK-NEXT:    init_existential_addr [[ENUM_PAYLOAD]] : $*Any, $()
// CHECK-NEXT:    apply %1()
// CHECK-NEXT:    inject_enum_addr %0 : $*Optional<Any>, #Optional.some!enumelt.1
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0S2iIxdd_ypIxr_TR : $@convention(thin) (@owned @callee_owned () -> (Int, Int)) -> @out Any
// CHECK:         [[ANY_PAYLOAD:%.*]] = init_existential_addr %0
// CHECK-NEXT:    [[LEFT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    [[RIGHT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    [[RESULT:%.*]] = apply %1()
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_extract [[RESULT]]
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_extract [[RESULT]]
// CHECK-NEXT:    store [[LEFT:%.*]] to [trivial] [[LEFT_ADDR]]
// CHECK-NEXT:    store [[RIGHT:%.*]] to [trivial] [[RIGHT_ADDR]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0S2iIxdd_ypSgIxr_TR : $@convention(thin) (@owned @callee_owned () -> (Int, Int)) -> @out Optional<Any> {
// CHECK:         [[OPTIONAL_PAYLOAD:%.*]] = init_enum_data_addr %0
// CHECK-NEXT:    [[ANY_PAYLOAD:%.*]] = init_existential_addr [[OPTIONAL_PAYLOAD]]
// CHECK-NEXT:    [[LEFT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    [[RIGHT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    [[RESULT:%.*]] = apply %1()
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_extract [[RESULT]]
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_extract [[RESULT]]
// CHECK-NEXT:    store [[LEFT:%.*]] to [trivial] [[LEFT_ADDR]]
// CHECK-NEXT:    store [[RIGHT:%.*]] to [trivial] [[RIGHT_ADDR]]
// CHECK-NEXT:    inject_enum_addr %0
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0ypIxi_S2iIxyy_TR : $@convention(thin) (Int, Int, @owned @callee_owned (@in Any) -> ()) -> ()
// CHECK:         [[ANY_VALUE:%.*]] = alloc_stack $Any
// CHECK-NEXT:    [[ANY_PAYLOAD:%.*]] = init_existential_addr [[ANY_VALUE]]
// CHECK-NEXT:    [[LEFT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    store %0 to [trivial] [[LEFT_ADDR]]
// CHECK-NEXT:    [[RIGHT_ADDR:%.*]] = tuple_element_addr [[ANY_PAYLOAD]]
// CHECK-NEXT:    store %1 to [trivial] [[RIGHT_ADDR]]
// CHECK-NEXT:    apply %2([[ANY_VALUE]])
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    dealloc_stack [[ANY_VALUE]]
// CHECK-NEXT:    return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0ypSgIxi_S2iIxyy_TR : $@convention(thin) (Int, Int, @owned @callee_owned (@in Optional<Any>) -> ()) -> ()
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
// CHECK-NEXT:    dealloc_stack [[OPTIONAL_VALUE]]
// CHECK-NEXT:    dealloc_stack [[ANY_VALUE]]
// CHECK-NEXT:    return
