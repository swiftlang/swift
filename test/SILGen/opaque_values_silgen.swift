// XFAIL: *

// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

struct TrivialStruct {
  var x: Int
}

protocol Foo {
  func foo()
}

protocol P {
  var x : Int { get }
}

protocol P2 : P {}

extension TrivialStruct : P2 {}

struct Box<T> {
  let t: T
}

protocol EmptyP {}

struct AddressOnlyStruct : EmptyP {}

struct AnyStruct {
  let a: Any
}

protocol Clonable {
  func maybeClone() -> Self?
}

indirect enum IndirectEnum<T> {
  case Nil
  case Node(T)
}

protocol SubscriptableGet {
  subscript(a : Int) -> Int { get }
}

protocol SubscriptableGetSet {
  subscript(a : Int) -> Int { get set }
}

var subscriptableGet : SubscriptableGet
var subscriptableGetSet : SubscriptableGetSet

class OpaqueClass<T> {
  typealias ObnoxiousTuple = (T, (T.Type, (T) -> T))

  func inAndOut(x: T) -> T { return x }
  func variantOptionalityTuples(x: ObnoxiousTuple) -> ObnoxiousTuple? { return x }
}

class StillOpaqueClass<T>: OpaqueClass<T> {
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

class OpaqueTupleClass<U>: OpaqueClass<(U, U)> {
  override func inAndOut(x: (U, U)) -> (U, U) { return x }
}

func unreachableF<T>() -> (Int, T)? { }

func s010_hasVarArg(_ args: Any...) {}

// Tests Address only enums's construction
// CHECK-LABEL: sil shared [transparent] @$S20opaque_values_silgen15AddressOnlyEnumO4mereyAcA6EmptyP_pcACmF : $@convention(method) (@in EmptyP, @thin AddressOnlyEnum.Type) -> @out AddressOnlyEnum {
// CHECK: bb0([[ARG0:%.*]] : $EmptyP, [[ARG1:%.*]] : $@thin AddressOnlyEnum.Type):
// CHECK:   [[RETVAL:%.*]] = enum $AddressOnlyEnum, #AddressOnlyEnum.mere!enumelt.1, [[ARG0]] : $EmptyP
// CHECK:   return [[RETVAL]] : $AddressOnlyEnum
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen15AddressOnlyEnumO4mereyAcA6EmptyP_pcACmF'

// CHECK-LABEL: sil shared [transparent] [thunk] @$S20opaque_values_silgen15AddressOnlyEnumO4mereyAcA6EmptyP_pcACmFTc : $@convention(thin) (@thin AddressOnlyEnum.Type) -> @owned @callee_guaranteed (@in_guaranteed EmptyP) -> @out AddressOnlyEnum {
// CHECK: bb0([[ARG:%.*]] : $@thin AddressOnlyEnum.Type):
// CHECK:   [[RETVAL:%.*]] = partial_apply {{.*}}([[ARG]]) : $@convention(method) (@in EmptyP, @thin AddressOnlyEnum.Type) -> @out AddressOnlyEnum
// CHECK:   [[CANONICAL_THUNK_FN:%.*]] = function_ref @$S20opaque_values_silgen6EmptyP_pAA15AddressOnlyEnumOIegir_AaB_pADIegnr_TR : $@convention(thin) (@in_guaranteed EmptyP, @guaranteed @callee_guaranteed (@in EmptyP) -> @out AddressOnlyEnum) -> @out AddressOnlyEnum
// CHECK:   [[CANONICAL_THUNK:%.*]] = partial_apply [callee_guaranteed] [[CANONICAL_THUNK_FN]]([[RETVAL]])
// CHECK:   return [[CANONICAL_THUNK]] : $@callee_guaranteed (@in_guaranteed EmptyP) -> @out AddressOnlyEnum
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen15AddressOnlyEnumO4mereyAcA6EmptyP_pcACmFTc'
enum AddressOnlyEnum {
  case nought
  case mere(EmptyP)
  case phantom(AddressOnlyStruct)
}

// Test vtables - OpaqueTupleClass
// ---
// CHECK-LABEL: sil private @$S20opaque_values_silgen16OpaqueTupleClassC8inAndOut1xx_xtx_xt_tFAA0dF0CAdExx_tFTV : $@convention(method) <U> (@in_guaranteed (U, U), @guaranteed OpaqueTupleClass<U>) -> @out (U, U) {
// CHECK: bb0([[ARG0:%.*]] : $(U, U), [[ARG1:%.*]] : $OpaqueTupleClass<U>):
// CHECK:   ([[TELEM0:%.*]], [[TELEM1:%.*]]) = destructure_tuple [[ARG0]] : $(U, U)
// CHECK:   [[APPLY:%.*]] = apply {{.*}}<U>([[TELEM0]], [[TELEM1]], [[ARG1]]) : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @guaranteed OpaqueTupleClass<τ_0_0>) -> (@out τ_0_0, @out τ_0_0)
// CHECK:   [[BORROWED_CALL:%.*]] = begin_borrow [[APPLY]]
// CHECK:   [[BORROWED_CALL_EXT0:%.*]] = tuple_extract [[BORROWED_CALL]] : $(U, U), 0
// CHECK:   [[RETVAL0:%.*]] = copy_value [[BORROWED_CALL_EXT0]] : $U
// CHECK:   [[BORROWED_CALL_EXT1:%.*]] = tuple_extract [[BORROWED_CALL]] : $(U, U), 1
// CHECK:   [[RETVAL1:%.*]] = copy_value [[BORROWED_CALL_EXT1]] : $U
// CHECK:   end_borrow [[BORROWED_CALL]]
// CHECK:   [[RETVAL:%.*]] = tuple ([[RETVAL0]] : $U, [[RETVAL1]] : $U)
// CHECK:   return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen16OpaqueTupleClassC8inAndOut1xx_xtx_xt_tFAA0dF0CAdExx_tFTV'

// Test vtables - StillOpaqueClass
// ---
// CHECK-LABEL: sil private @$S20opaque_values_silgen16StillOpaqueClassC24variantOptionalityTuples1xx_xm_xxcttx_xm_xxcttSg_tFAA0eF0CAdeFx_xm_xxctt_tFTV : $@convention(method) <T> (@in_guaranteed T, @thick T.Type, @guaranteed @callee_guaranteed (@in_guaranteed T) -> @out T, @guaranteed StillOpaqueClass<T>) -> @out Optional<(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))> {
// CHECK: bb0([[ARG0:%.*]] : $T, [[ARG1:%.*]] : $@thick T.Type, [[ARG2:%.*]] : $@callee_guaranteed (@in_guaranteed T) -> @out T, [[ARG3:%.*]] : $StillOpaqueClass<T>):
// CHECK:   [[TELEM0:%.*]] = tuple ([[ARG1]] : $@thick T.Type, [[ARG2]] : $@callee_guaranteed (@in_guaranteed T) -> @out T)
// CHECK:   [[TELEM1:%.*]] = tuple ([[ARG0]] : $T, [[TELEM0]] : $(@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))
// CHECK:   [[ENUMOPT0:%.*]] = enum $Optional<(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))>, #Optional.some!enumelt.1, [[TELEM1]] : $(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))
// CHECK:   [[APPLY:%.*]] = apply {{.*}}<T>([[ENUMOPT0]], [[ARG3]]) : $@convention(method) <τ_0_0> (@in_guaranteed Optional<(τ_0_0, (@thick τ_0_0.Type, @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0))>, @guaranteed StillOpaqueClass<τ_0_0>) -> (@out τ_0_0, @thick τ_0_0.Type, @owned @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0)
// CHECK:   [[BORROWED_T:%.*]] = begin_borrow [[APPLY]]
// CHECK:   [[BORROWED_T_EXT0:%.*]] = tuple_extract [[BORROWED_T]] : $(T, @thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T), 0
// CHECK:   [[RETVAL0:%.*]] = copy_value [[BORROWED_T_EXT0]]
// CHECK:   [[BORROWED_T_EXT1:%.*]] = tuple_extract [[BORROWED_T]] : $(T, @thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T), 1
// CHECK:   [[BORROWED_T_EXT2:%.*]] = tuple_extract [[BORROWED_T]] : $(T, @thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T), 2
// CHECK:   [[RETVAL1:%.*]] = copy_value [[BORROWED_T_EXT2]]
// CHECK:   end_borrow [[BORROWED_T]]
// CHECK:   [[RETTUPLE0:%.*]] = tuple ([[BORROWED_T_EXT1]] : $@thick T.Type, [[RETVAL1]] : $@callee_guaranteed (@in_guaranteed T) -> @out T)
// CHECK:   [[RETTUPLE1:%.*]] = tuple ([[RETVAL0]] : $T, [[RETTUPLE0]] : $(@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))
// CHECK:   [[RETVAL:%.*]] = enum $Optional<(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))>, #Optional.some!enumelt.1, [[RETTUPLE1]] : $(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))
// CHECK:   return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen16StillOpaqueClassC24variantOptionalityTuples1xx_xm_xxcttx_xm_xxcttSg_tFAA0eF0CAdeFx_xm_xxctt_tFTV'


// part of s280_convExistTrivial: conversion between existential types - reabstraction thunk
// ---
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$S20opaque_values_silgen1P_pAA13TrivialStructVIegnd_AA2P2_pAaE_pIegnr_TR : $@convention(thin) (@in_guaranteed P2, @guaranteed @callee_guaranteed (@in_guaranteed P) -> TrivialStruct) -> @out P2 {
// CHECK: bb0([[ARG0:%.*]] : $P2, [[ARG1:%.*]] : $@callee_guaranteed (@in_guaranteed P) -> TrivialStruct):
// CHECK:   [[OPENED_ARG:%.*]] = open_existential_value [[ARG]] : $P2 to $@opened({{.*}}) P2
// CHECK:   [[COPIED_VAL:%.*]] = copy_value [[OPENED_ARG]]
// CHECK:   [[INIT_P:%.*]] = init_existential_value [[COPIED_VAL]] : $@opened({{.*}}) P2, $@opened({{.*}}) P2, $P
// CHECK:   [[BORROWED_INIT_P:%.*]] = begin_borrow [[INIT_P]]
// CHECK:   [[APPLY_P:%.*]] = apply [[ARG1]]([[BORROWED_INIT_P]]) : $@callee_guaranteed (@in_guaranteed P) -> TrivialStruct
// CHECK:   [[RETVAL:%.*]] = init_existential_value [[APPLY_P]] : $TrivialStruct, $TrivialStruct, $P2
// CHECK:   end_borrow [[BORROWED_INIT_P]]
// CHECK-NOT:   destroy_value [[ARG0]]
// CHECK:   return [[RETVAL]] : $P2
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen1P_pAA13TrivialStructVIegnd_AA2P2_pAaE_pIegnr_TR'

// part of s290_convOptExistTriv: conversion between existential types - reabstraction thunk - optionals case
// ---
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$S20opaque_values_silgen1P_pSgAA13TrivialStructVIegnd_AESgAA2P2_pIegyr_TR : $@convention(thin) (Optional<TrivialStruct>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<P>) -> TrivialStruct) -> @out P2 {
// CHECK: bb0([[ARG0:%.*]] : $Optional<TrivialStruct>, [[ARG1:%.*]] : $@callee_guaranteed (@in_guaranteed Optional<P>) -> TrivialStruct):
// CHECK:   switch_enum [[ARG0]] : $Optional<TrivialStruct>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb1:
// CHECK:   [[ONONE:%.*]] = enum $Optional<P>, #Optional.none!enumelt
// CHECK:   br bb3([[ONONE]] : $Optional<P>)
// CHECK: bb2([[OSOME:%.*]] : $TrivialStruct):
// CHECK:   [[INIT_S:%.*]] = init_existential_value [[OSOME]] : $TrivialStruct, $TrivialStruct, $P
// CHECK:   [[ENUM_S:%.*]] = enum $Optional<P>, #Optional.some!enumelt.1, [[INIT_S]] : $P
// CHECK:   br bb3([[ENUM_S]] : $Optional<P>)
// CHECK: bb3([[OPT_S:%.*]] : $Optional<P>):
// CHECK:   [[BORROWED_OPT_S:%.*]] = begin_borrow [[OPT_S]]
// CHECK:   [[APPLY_P:%.*]] = apply [[ARG1]]([[BORROWED_OPT_S]]) : $@callee_guaranteed (@in_guaranteed Optional<P>) -> TrivialStruct
// CHECK:   [[RETVAL:%.*]] = init_existential_value [[APPLY_P]] : $TrivialStruct, $TrivialStruct, $P2
// CHECK:   return [[RETVAL]] : $P2
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen1P_pSgAA13TrivialStructVIegnd_AESgAA2P2_pIegyr_TR'

// Test array initialization - we are still (somewhat) using addresses
// ---
// CHECK-LABEL: sil @$S20opaque_values_silgen21s020_______callVarArgyyF : $@convention(thin) () -> () {
// CHECK: %[[APY:.*]] = apply %{{.*}}<Any>(%{{.*}}) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: %[[BRW:.*]] = begin_borrow %[[APY]]
// CHECK: %[[TPL:.*]] = tuple_extract %[[BRW]] : $(Array<Any>, Builtin.RawPointer), 1
// CHECK: end_borrow %[[BRW]] from %[[APY]] : $(Array<Any>, Builtin.RawPointer), $(Array<Any>, Builtin.RawPointer)
// CHECK: destroy_value %[[APY]]
// CHECK: %[[PTR:.*]] = pointer_to_address %[[TPL]] : $Builtin.RawPointer to [strict] $*Any
// CHECK: [[IOPAQUE:%.*]] = init_existential_value %{{.*}} : $Int, $Int, $Any
// CHECK: store [[IOPAQUE]] to [init] %[[PTR]] : $*Any
// CHECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s020_______callVarArgyyF'
public func s020_______callVarArg() {
  s010_hasVarArg(3)
}

// Test emitSemanticStore.
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s030______assigninoutyyxz_xtlF : $@convention(thin) <T> (@inout T, @in_guaranteed T) -> () {
// CHECK: bb0([[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $T):
// CHECK:   [[CPY:%.*]] = copy_value [[ARG1]] : $T
// CHECK:   [[READ:%.*]] = begin_access [modify] [unknown] [[ARG0]] : $*T
// CHECK:   assign [[CPY]] to [[READ]] : $*T
// CHECK-NOT:   destroy_value [[ARG1]] : $T
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s030______assigninoutyyxz_xtlF'
func s030______assigninout<T>(_ a: inout T, _ b: T) {
  a = b
}

// Test that we no longer use copy_addr or tuple_element_addr when copy by value is possible
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s040___tupleReturnIntyS2i_xt_tlF : $@convention(thin) <T> (Int, @in_guaranteed T) -> Int {
// CHECK: bb0([[ARG0:%.*]] : $Int, [[ARG1:%.*]] : $T):
// CHECK:   [[ARG1_COPY:%.*]] = copy_value [[ARG1]]
// CHECK:   [[TPL:%.*]] = tuple ([[ARG0]] : $Int, [[ARG1_COPY]] : $T)
// CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[TPL]] : $(Int, T)
// CHECK:   [[CPY:%.*]] = copy_value [[BORROWED_ARG1]] : $(Int, T)
// CHECK:   [[BORROWED_CPY:%.*]] = begin_borrow [[CPY]]
// CHECK:   [[INT:%.*]] = tuple_extract [[BORROWED_CPY]] : $(Int, T), 0
// CHECK:   [[GEN:%.*]] = tuple_extract [[BORROWED_CPY]] : $(Int, T), 1
// CHECK:   [[COPY_GEN:%.*]] = copy_value [[GEN]]
// CHECK:   destroy_value [[COPY_GEN]]
// CHECK:   end_borrow [[BORROWED_CPY]] from [[CPY]]
// CHECK:   destroy_value [[CPY]]
// CHECK:   end_borrow [[BORROWED_ARG1]] from [[TPL]] : $(Int, T), $(Int, T)
// CHECK:   destroy_value [[TPL]] : $(Int, T)
// CHECK:   return [[INT]]
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s040___tupleReturnIntyS2i_xt_tlF'
func s040___tupleReturnInt<T>(_ x: (Int, T)) -> Int {
  let y = x.0
  return y
}

// Test returning an opaque tuple of tuples.
// ---
// CHECK-LABEL: sil hidden [noinline] @$S20opaque_values_silgen21s050______multiResultyx_x_xttxlF : $@convention(thin) <T> (@in_guaranteed T) -> (@out T, @out T, @out T) {
// CHECK: bb0(%0 : $T):
// CHECK: %[[CP1:.*]] = copy_value %{{.*}} : $T
// CHECK: %[[CP2:.*]] = copy_value %{{.*}} : $T
// CHECK: %[[CP3:.*]] = copy_value %{{.*}} : $T
// CHECK-NOT: destroy_value %0 : $T
// CHECK: %[[TPL:.*]] = tuple (%[[CP1]] : $T, %[[CP2]] : $T, %[[CP3]] : $T)
// CHECK: return %[[TPL]] : $(T, T, T)
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s050______multiResultyx_x_xttxlF'
@inline(never)
func s050______multiResult<T>(_ t: T) -> (T, (T, T)) {
  return (t, (t, t))
}

// Test returning an opaque tuple of tuples as a concrete tuple.
// ---
// CHECK-LABEL: sil @$S20opaque_values_silgen21s060__callMultiResult1iSi_Si_SittSi_tF : $@convention(thin) (Int) -> (Int, Int, Int) {
// CHECK: bb0(%0 : $Int):
// CHECK: %[[FN:.*]] = function_ref @$S20opaque_values_silgen21s050______multiResultyx_x_xttxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @out τ_0_0, @out τ_0_0)
// CHECK: %[[TPL:.*]] = apply %[[FN]]<Int>(%0) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @out τ_0_0, @out τ_0_0)
// CHECK: %[[I1:.*]] = tuple_extract %[[TPL]] : $(Int, Int, Int), 0
// CHECK: %[[I2:.*]] = tuple_extract %[[TPL]] : $(Int, Int, Int), 1
// CHECK: %[[I3:.*]] = tuple_extract %[[TPL]] : $(Int, Int, Int), 2
// CHECK: %[[R:.*]] = tuple (%[[I1]] : $Int, %[[I2]] : $Int, %[[I3]] : $Int)
// CHECK: return %[[R]] : $(Int, Int, Int)
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s060__callMultiResult1iSi_Si_SittSi_tF'
public func s060__callMultiResult(i: Int) -> (Int, (Int, Int)) {
  return s050______multiResult(i)
}

// SILGen, prepareArchetypeCallee. Materialize a
// non-class-constrainted self from a class-constrained archetype.
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s070__materializeSelf1tyx_tRlzCAA3FooRzlF : $@convention(thin) <T where T : AnyObject, T : Foo> (@guaranteed T) -> () {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK: [[WITNESS_METHOD:%.*]] = witness_method $T, #Foo.foo!1 : <Self where Self : Foo> (Self) -> () -> () : $@convention(witness_method: Foo) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply [[WITNESS_METHOD]]<T>([[ARG]]) : $@convention(witness_method: Foo) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()
// CHECK-NOT: destroy_value [[ARG]] : $T
// CHECK: return %{{[0-9]+}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s070__materializeSelf1tyx_tRlzCAA3FooRzlF'
func s070__materializeSelf<T: Foo>(t: T) where T: AnyObject {
  t.foo()
}

// Test open existential with opaque values
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s080______________bar1pSiAA1P_p_tF : $@convention(thin) (@in_guaranteed P) -> Int {
// CHECK: bb0([[ARG:%.*]] : $P):
// CHECK:   [[OPENED_ARG:%.*]] = open_existential_value [[ARG]] : $P to $@opened
// CHECK:   [[WITNESS_FUNC:%.*]] = witness_method $@opened
// CHECK:   [[RESULT:%.*]] = apply [[WITNESS_FUNC]]<{{.*}}>([[OPENED_ARG]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> Int
// CHECK-NOT:   destroy_value [[ARG]] : $P
// CHECK:   return [[RESULT]] : $Int
func s080______________bar(p: P) -> Int {
  return p.x
}

// Test OpaqueTypeLowering copyValue and destroyValue.
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s090___________calleryxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK-NOT: copy_value
// CHECK:   [[RESULT:%.*]] = apply {{%.*}}<T>([[ARG]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK-NOT:   destroy_value [[ARG]] : $T
// CHECK:   return %{{.*}} : $T
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s090___________calleryxxlF'
func s090___________caller<T>(_ t: T) -> T {
  return s090___________caller(t)
}

// Test a simple opaque parameter and return value.
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s100_________identityyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]] : $T
// CHECK-NOT:   destroy_value [[ARG]] : $T
// CHECK:   return [[COPY_ARG]] : $T
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s100_________identityyxxlF'
func s100_________identity<T>(_ t: T) -> T {
  return t
}

// Test a guaranteed opaque parameter.
// ---
// CHECK-LABEL: sil private [transparent] [thunk] @$S20opaque_values_silgen21s110___GuaranteedSelfVAA3FooA2aDP3fooyyFTW : $@convention(witness_method: Foo) (@in_guaranteed s110___GuaranteedSelf) -> () {
// CHECK: bb0(%0 : $s110___GuaranteedSelf):
// CHECK:   %[[F:.*]] = function_ref @$S20opaque_values_silgen21s110___GuaranteedSelfV3fooyyF : $@convention(method) (s110___GuaranteedSelf) -> ()
// CHECK:   apply %[[F]](%0) : $@convention(method) (s110___GuaranteedSelf) -> ()
// CHECK:   return
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s110___GuaranteedSelfVAA3FooA2aDP3fooyyFTW'
struct s110___GuaranteedSelf : Foo {
  func foo() {}
}

// Tests a corner case wherein we used to do a temporary and return a pointer to T instead of T
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s120______returnValueyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[COPY_ARG1:%.*]] = copy_value [[ARG]] : $T
// CHECK:   [[BORROWED_ARG2:%.*]] = begin_borrow [[COPY_ARG1]]
// CHECK:   [[COPY_ARG2:%.*]] = copy_value [[BORROWED_ARG2]] : $T
// CHECK:   end_borrow [[BORROWED_ARG2]] from [[COPY_ARG1]]
// CHECK:   return [[COPY_ARG2]] : $T
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s120______returnValueyxxlF'
func s120______returnValue<T>(_ x: T) -> T {
  let y = x
  return y
}

// Tests Optional initialization by value
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s130_____________wrapyxSgxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out Optional<T> {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]] : $T
// CHECK:   [[OPTIONAL_ARG:%.*]] = enum $Optional<T>, #Optional.some!enumelt.1, [[COPY_ARG]] : $T
// CHECK-NOT:   destroy_value [[ARG]] : $T
// CHECK:   return [[OPTIONAL_ARG]] : $Optional<T>
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s130_____________wrapyxSgxlF'
func s130_____________wrap<T>(_ x: T) -> T? {
  return x
}

// Tests For-each statements
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s140______forEachStmtyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[PROJ_BOX_ARG:%.*]] = project_box %{{.*}} : ${ var IndexingIterator<Range<Int>> }
// CHECK:   [[APPLY_ARG1:%.*]] = apply
// CHECK-NOT: alloc_stack $Int
// CHECK-NOT: store [[APPLY_ARG1]] to [trivial]
// CHECK-NOT: alloc_stack $Range<Int>
// CHECK-NOT: dealloc_stack
// CHECK:   [[APPLY_ARG2:%.*]] = apply %{{.*}}<Range<Int>>
// CHECK:   store [[APPLY_ARG2]] to [trivial] [[PROJ_BOX_ARG]]
// CHECK:   br bb1
// CHECK: bb1:
// CHECK-NOT: alloc_stack $Optional<Int>
// CHECK:   [[APPLY_ARG3:%.*]] = apply %{{.*}}<Range<Int>>
// CHECK-NOT: dealloc_stack
// CHECK:   switch_enum [[APPLY_ARG3]]
// CHECK: bb2:
// CHECK:   br bb3
// CHECK: bb3:
// CHECK:   return %{{.*}} : $()
// CHECK: bb4([[ENUM_ARG:%.*]] : $Int):
// CHECK-NOT:   unchecked_enum_data
// CHECK:   br bb1
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s140______forEachStmtyyF'
func s140______forEachStmt() {
  for _ in 1..<42 {
  }
}

func s150___________anyArg(_: Any) {}

// Tests init of opaque existentials
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s160_______callAnyArgyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[INT_TYPE:%.*]] = metatype $@thin Int.Type
// CHECK:   [[INT_LIT:%.*]] = integer_literal $Builtin.Int2048, 42
// CHECK:   [[INT_ARG:%.*]] = apply %{{.*}}([[INT_LIT]], [[INT_TYPE]]) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[INT_ARG]] : $Int, $Int, $Any
// CHECK:   apply %{{.*}}([[INIT_OPAQUE]]) : $@convention(thin) (@in_guaranteed Any) -> ()
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s160_______callAnyArgyyF'
func s160_______callAnyArg() {
  s150___________anyArg(42)
}

// Tests unconditional_checked_cast for opaque values
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s170____force_convertxylF : $@convention(thin) <T> () -> @out T {
// CHECK: bb0:
// CHECK-NOT: alloc_stack
// CHECK:   [[INT_TYPE:%.*]] = metatype $@thin Int.Type
// CHECK:   [[INT_LIT:%.*]] = integer_literal $Builtin.Int2048, 42
// CHECK:   [[INT_ARG:%.*]] = apply %{{.*}}([[INT_LIT]], [[INT_TYPE]]) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK:   [[INT_CAST:%.*]] = unconditional_checked_cast_value [[INT_ARG]] : $Int to $T
// CHECK:   [[CAST_BORROW:%.*]] = begin_borrow [[INT_CAST]] : $T
// CHECK:   [[RETURN_VAL:%.*]] = copy_value [[CAST_BORROW]] : $T
// CHECK:   end_borrow [[CAST_BORROW]] from [[INT_CAST]] : $T, $T
// CHECK:   destroy_value [[INT_CAST]] : $T
// CHECK:   return [[RETURN_VAL]] : $T
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s170____force_convertxylF'
func s170____force_convert<T>() -> T {
  let x : T = 42 as! T
  return x
}

// Tests supporting function for s190___return_foo_var - cast and return of protocol
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s180_______return_fooAA3Foo_pyF : $@convention(thin) () -> @out Foo {
// CHECK: bb0:
// CHECK:   [[INT_LIT:%.*]] = integer_literal $Builtin.Int2048, 42
// CHECK:   [[INT_ARG:%.*]] = apply %{{.*}}([[INT_LIT]], [[INT_TYPE]]) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK:   [[INT_CAST:%.*]] = unconditional_checked_cast_value [[INT_ARG]] : $Int to $Foo
// CHECK:   return [[INT_CAST]] : $Foo
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s180_______return_fooAA3Foo_pyF'
func s180_______return_foo() -> Foo {
  return 42 as! Foo
}
var foo_var : Foo = s180_______return_foo()

// Tests return of global variables by doing a load of copy
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s190___return_foo_varAA3Foo_pyF : $@convention(thin) () -> @out Foo {
// CHECK: bb0:
// CHECK:   [[GLOBAL:%.*]] = global_addr {{.*}} : $*Foo
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL]] : $*Foo
// CHECK:   [[LOAD_GLOBAL:%.*]] = load [copy] [[READ]] : $*Foo
// CHECK:   return [[LOAD_GLOBAL]] : $Foo
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s190___return_foo_varAA3Foo_pyF'
func s190___return_foo_var() -> Foo {
  return foo_var
}

// Tests deinit of opaque existentials
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s200______use_foo_varyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[GLOBAL:%.*]] = global_addr {{.*}} : $*Foo
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL]] : $*Foo
// CHECK:   [[LOAD_GLOBAL:%.*]] = load [copy] [[READ]] : $*Foo
// CHECK:   [[BORROW:%.*]] = begin_borrow [[LOAD_GLOBAL]] : $Foo
// CHECK:   [[OPEN_VAR:%.*]] = open_existential_value [[BORROW]] : $Foo
// CHECK:   [[WITNESS:%.*]] = witness_method $@opened
// CHECK:   apply [[WITNESS]]
// CHECK:   end_borrow [[BORROW]]
// CHECK:   destroy_value [[LOAD_GLOBAL]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s200______use_foo_varyyF'
func s200______use_foo_var() {
  foo_var.foo()
}

// Tests composition erasure of opaque existentials + copy into of opaques
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s210______compErasureys5Error_psAC_AA3FoopF : $@convention(thin) (@in_guaranteed Error & Foo) -> @owned Error {
// CHECK: bb0([[ARG:%.*]] : $Error & Foo):
// CHECK:   [[OPAQUE_ARG:%.*]] = open_existential_value [[ARG]] : $Error & Foo to $@opened({{.*}}) Error & Foo
// CHECK:   [[EXIST_BOX:%.*]] = alloc_existential_box $Error, $@opened({{.*}}) Error & Foo
// CHECK:   [[PROJ_BOX:%.*]] = project_existential_box $@opened({{.*}}) Error & Foo in [[EXIST_BOX]]
// CHECK:   [[COPY_OPAQUE:%.*]] = copy_value [[OPAQUE_ARG]] : $@opened({{.*}}) Error & Foo
// CHECK:   store [[COPY_OPAQUE]] to [init] [[PROJ_BOX]] : $*@opened({{.*}}) Error & Foo
// CHECK-NOT:   destroy_value [[ARG]] : $Error & Foo
// CHECK:   return [[EXIST_BOX]] : $Error
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s210______compErasureys5Error_psAC_AA3FoopF'
func s210______compErasure(_ x: Foo & Error) -> Error {
  return x
}

// Tests that existential boxes can contain opaque types
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s220_____openExistBoxySSs5Error_pF : $@convention(thin) (@guaranteed Error) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : $Error):
// CHECK:   [[OPAQUE_ARG:%.*]] = open_existential_box_value [[ARG]] : $Error to $@opened({{.*}}) Error
// CHECK:   [[ALLOC_OPEN:%.*]] = alloc_stack $@opened({{.*}}) Error
// CHECK:   store_borrow [[OPAQUE_ARG]] to [[ALLOC_OPEN]]
// CHECK:   dealloc_stack [[ALLOC_OPEN]]
// CHECK-NOT:   destroy_value [[ARG]] : $Error
// CHECK:   return {{.*}} : $String
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s220_____openExistBoxySSs5Error_pF'
func s220_____openExistBox(_ x: Error) -> String {
  return x._domain
}

// Tests conditional value casts and correspondingly generated reabstraction thunk
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s230______condFromAnyyyypF : $@convention(thin) (@in_guaranteed Any) -> () {
// CHECK: bb0([[ARG:%.*]] : $Any):
// CHECK:   [[COPY__ARG:%.*]] = copy_value [[ARG]]
// CHECK:   checked_cast_value_br [[COPY__ARG]] : $Any to $@callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int), bb2, bb1
// CHECK: bb2([[THUNK_PARAM:%.*]] : $@callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int)):
// CHECK:   [[THUNK_REF:%.*]] = function_ref @{{.*}} : $@convention(thin) (Int, Int, Int, Int, Int, @guaranteed @callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int)) -> (Int, Int, Int, Int, Int)
// CHECK:   partial_apply [callee_guaranteed] [[THUNK_REF]]([[THUNK_PARAM]])
// CHECK: bb6:
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s230______condFromAnyyyypF'
func s230______condFromAny(_ x: Any) {
  if let f = x as? (Int, (Int, (Int, Int)), Int) -> (Int, (Int, (Int, Int)), Int) {
    _ = f(24, (4,(2, 42)), 42)
  }
}

// Tests LValue of error types / existential boxes
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s240_____propOfLValueySSs5Error_pF : $@convention(thin) (@guaranteed Error) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : $Error):
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var Error }
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[ALLOC_OF_BOX]]
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   store [[COPY_ARG]] to [init] [[PROJ_BOX]]
// CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*Error
// CHECK:   [[LOAD_BOX:%.*]] = load [copy] [[READ]]
// CHECK:   [[OPAQUE_ARG:%.*]] = open_existential_box [[LOAD_BOX]] : $Error to $*@opened({{.*}}) Error
// CHECK:   [[LOAD_OPAQUE:%.*]] = load [copy] [[OPAQUE_ARG]]
// CHECK:   [[ALLOC_OPEN:%.*]] = alloc_stack $@opened({{.*}}) Error
// CHECK:   store [[LOAD_OPAQUE]] to [init] [[ALLOC_OPEN]]
// CHECK:   [[RET_VAL:%.*]] = apply {{.*}}<@opened({{.*}}) Error>([[ALLOC_OPEN]])
// CHECK:   return [[RET_VAL]] : $String
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s240_____propOfLValueySSs5Error_pF'
func s240_____propOfLValue(_ x: Error) -> String {
  var x = x
  return x._domain
}

// Tests Implicit Value Construction under Opaque value mode
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s250_________testBoxTyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[BOX_MTYPE:%.*]] = metatype $@thin Box<Int>.Type
// CHECK:   [[MTYPE:%.*]] = metatype $@thin Int.Type
// CHECK:   [[INTLIT:%.*]] = integer_literal $Builtin.Int2048, 42
// CHECK:   [[AINT:%.*]] = apply {{.*}}([[INTLIT]], [[MTYPE]]) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK:   apply {{.*}}<Int>([[AINT]], [[BOX_MTYPE]]) : $@convention(method) <τ_0_0> (@in τ_0_0, @thin Box<τ_0_0>.Type) -> @out Box<τ_0_0>
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s250_________testBoxTyyF'
func s250_________testBoxT() {
  let _ = Box(t: 42)
}

// Tests Address only enums
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s260_______AOnly_enumyyAA17AddressOnlyStructVF : $@convention(thin) (AddressOnlyStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : $AddressOnlyStruct):
// CHECK:   [[MTYPE1:%.*]] = metatype $@thin AddressOnlyEnum.Type
// CHECK:   [[APPLY1:%.*]] =  apply {{.*}}([[MTYPE1]]) : $@convention(thin) (@thin AddressOnlyEnum.Type) -> @owned @callee_guaranteed (@in_guaranteed EmptyP) -> @out AddressOnlyEnum
// CHECK:   destroy_value [[APPLY1]]
// CHECK:   [[MTYPE2:%.*]] = metatype $@thin AddressOnlyEnum.Type
// CHECK:   [[ENUM1:%.*]] = enum $AddressOnlyEnum, #AddressOnlyEnum.nought!enumelt
// CHECK:   [[MTYPE3:%.*]] = metatype $@thin AddressOnlyEnum.Type
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[ARG]] : $AddressOnlyStruct, $AddressOnlyStruct, $EmptyP
// CHECK:   [[ENUM2:%.*]] = enum $AddressOnlyEnum, #AddressOnlyEnum.mere!enumelt.1, [[INIT_OPAQUE]] : $EmptyP
// CHECK:   destroy_value [[ENUM2]]
// CHECK:   [[MTYPE4:%.*]] = metatype $@thin AddressOnlyEnum.Type
// CHECK:   [[ENUM3:%.*]] = enum $AddressOnlyEnum, #AddressOnlyEnum.phantom!enumelt.1, [[ARG]] : $AddressOnlyStruct
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s260_______AOnly_enumyyAA17AddressOnlyStructVF'
func s260_______AOnly_enum(_ s: AddressOnlyStruct) {
  _ = AddressOnlyEnum.mere

  _ = AddressOnlyEnum.nought

  _ = AddressOnlyEnum.mere(s)

  _ = AddressOnlyEnum.phantom(s)
}

// Tests InjectOptional for opaque value types + conversion of opaque structs
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s270_convOptAnyStructyyAA0gH0VADSgcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (@in_guaranteed Optional<AnyStruct>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct) -> @out Optional<AnyStruct>
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out Optional<AnyStruct>
// CHECK-NOT:   destroy_value [[ARG]] : $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s270_convOptAnyStructyyAA0gH0VADSgcF'
func s270_convOptAnyStruct(_ a1: @escaping (AnyStruct?) -> AnyStruct) {
  let _: (AnyStruct?) -> AnyStruct? = a1
}

// Tests conversion between existential types
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s280_convExistTrivialyyAA0G6StructVAA1P_pcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed P) -> TrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed (@in_guaranteed P) -> TrivialStruct):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (@in_guaranteed P2, @guaranteed @callee_guaranteed (@in_guaranteed P) -> TrivialStruct) -> @out P2
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed (@in_guaranteed P2) -> @out P2
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s280_convExistTrivialyyAA0G6StructVAA1P_pcF'
func s280_convExistTrivial(_ s: @escaping (P) -> TrivialStruct) {
  let _: (P2) -> P2 = s
}

// Tests conversion between existential types - optionals case
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s290_convOptExistTrivyyAA13TrivialStructVAA1P_pSgcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed Optional<P>) -> TrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed (@in_guaranteed Optional<P>) -> TrivialStruct):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (Optional<TrivialStruct>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<P>) -> TrivialStruct) -> @out P2
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed (Optional<TrivialStruct>) -> @out P2
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s290_convOptExistTrivyyAA13TrivialStructVAA1P_pSgcF'
func s290_convOptExistTriv(_ s: @escaping (P?) -> TrivialStruct) {
  let _: (TrivialStruct?) -> P2 = s
}

// Tests corner-case: reabstraction of an empty tuple to any
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s300__convETupleToAnyyyyycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed () -> ()):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @out Any
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed () -> @out Any
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s300__convETupleToAnyyyyycF'
func s300__convETupleToAny(_ t: @escaping () -> ()) {
  let _: () -> Any = t
}

// Tests corner-case: reabstraction of a non-empty tuple to any
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s310__convIntTupleAnyyySi_SitycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> (Int, Int)) -> () {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed () -> (Int, Int)):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (@guaranteed @callee_guaranteed () -> (Int, Int)) -> @out Any
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed () -> @out Any
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s310__convIntTupleAnyyySi_SitycF'
func s310__convIntTupleAny(_ t: @escaping () -> (Int, Int)) {
  let _: () -> Any = t
}

// Tests translating and imploding into Any under opaque value mode
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s320__transImplodeAnyyyyypcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed Any) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed (@in_guaranteed Any) -> ()):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (Int, Int, @guaranteed @callee_guaranteed (@in_guaranteed Any) -> ()) -> ()
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed (Int, Int) -> ()
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s320__transImplodeAnyyyyypcF'
func s320__transImplodeAny(_ t: @escaping (Any) -> ()) {
  let _: ((Int, Int)) -> () = t
}

// Tests support for address only let closures under opaque value mode - they are not by-address anymore
// ---
// CHECK-LABEL: sil private @$S20opaque_values_silgen21s330___addrLetClosureyxxlFxyXEfU_xyXEfU_ : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]] : $T
// CHECK:   return [[COPY_ARG]] : $T
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s330___addrLetClosureyxxlFxyXEfU_xyXEfU_'
func s330___addrLetClosure<T>(_ x:T) -> T {
  return { { x }() }()
}

// Tests support for capture of a mutable opaque value type
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s340_______captureBoxyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var EmptyP }, var, name "mutableAddressOnly"
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[ALLOC_OF_BOX]]
// CHECK:   [[APPLY_FOR_BOX:%.*]] = apply %{{.*}}(%{{.*}}) : $@convention(method) (@thin AddressOnlyStruct.Type) -> AddressOnlyStruct
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[APPLY_FOR_BOX]] : $AddressOnlyStruct, $AddressOnlyStruct, $EmptyP
// CHECK:   store [[INIT_OPAQUE]] to [init] [[PROJ_BOX]] : $*EmptyP
// CHECK:   [[BORROW_BOX:%.*]] = begin_borrow [[ALLOC_OF_BOX]] : ${ var EmptyP }
// CHECK:   mark_function_escape [[PROJ_BOX]] : $*EmptyP
// CHECK:   apply %{{.*}}([[BORROW_BOX]]) : $@convention(thin) (@guaranteed { var EmptyP }) -> ()
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s340_______captureBoxyyF'
func s340_______captureBox() {
  var mutableAddressOnly: EmptyP = AddressOnlyStruct()

  func captureEverything() {
    _ = s100_________identity((mutableAddressOnly))
  }

  captureEverything()
}

// Tests support for if statements for opaque value(s) under new mode
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s350_______addrOnlyIf1xAA6EmptyP_pSb_tF : $@convention(thin) (Bool) -> @out EmptyP {
// CHECK: bb0([[ARG:%.*]] : $Bool):
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var EmptyP }, var
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[ALLOC_OF_BOX]]
// CHECK:   [[APPLY_FOR_BOX:%.*]] = apply %{{.*}}(%{{.*}}) : $@convention(method) (@thin AddressOnlyStruct.Type) -> AddressOnlyStruct
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[APPLY_FOR_BOX]] : $AddressOnlyStruct, $AddressOnlyStruct, $EmptyP
// CHECK:   store [[INIT_OPAQUE]] to [init] [[PROJ_BOX]] : $*EmptyP
// CHECK:   [[APPLY_FOR_BRANCH:%.*]] = apply %{{.*}}([[ARG]]) : $@convention(method) (Bool) -> Builtin.Int1
// CHECK:   cond_br [[APPLY_FOR_BRANCH]], bb2, bb1
// CHECK: bb1:
// CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*EmptyP
// CHECK:   [[RETVAL1:%.*]] = load [copy] [[READ]] : $*EmptyP
// CHECK:   br bb3([[RETVAL1]] : $EmptyP)
// CHECK: bb2:
// CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*EmptyP
// CHECK:   [[RETVAL2:%.*]] = load [copy] [[READ]] : $*EmptyP
// CHECK:   br bb3([[RETVAL2]] : $EmptyP)
// CHECK: bb3([[RETVAL:%.*]] : $EmptyP):
// CHECK:   destroy_value [[ALLOC_OF_BOX]]
// CHECK:   return [[RETVAL]] : $EmptyP
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s350_______addrOnlyIf1xAA6EmptyP_pSb_tF'
func s350_______addrOnlyIf(x: Bool) -> EmptyP {
  var a : EmptyP = AddressOnlyStruct()

  return x ? a : a
}

// Tests support for guards and indirect enums for opaque values
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s360________guardEnumyyAA08IndirectF0OyxGlF : $@convention(thin) <T> (@guaranteed IndirectEnum<T>) -> () {
// CHECK: bb0([[ARG:%.*]] : $IndirectEnum<T>):
// CHECK:   [[COPY__ARG:%.*]] = copy_value [[ARG]]
// CHECK:   switch_enum [[COPY__ARG]] : $IndirectEnum<T>, case #IndirectEnum.Node!enumelt.1: [[NODE_BB:bb[0-9]+]], case #IndirectEnum.Nil!enumelt: [[NIL_BB:bb[0-9]+]]
//
// CHECK: [[NIL_BB]]:
// CHECK:   br [[NIL_TRAMPOLINE:bb[0-9]+]]
//
// CHECK: [[NIL_TRAMPOLINE]]:
// CHECK:   br [[EPILOG_BB:bb[0-9]+]]
//
// CHECK: [[NODE_BB]]([[EARG:%.*]] : $<τ_0_0> { var τ_0_0 } <T>):
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[EARG]]
// CHECK:   [[LOAD_BOX:%.*]] = load [take] [[PROJ_BOX]] : $*T
// CHECK:   [[COPY_BOX:%.*]] = copy_value [[LOAD_BOX]] : $T
// CHECK:   destroy_value [[EARG]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]
//
// CHECK: [[CONT_BB]]:
// CHECK:   destroy_value [[COPY_BOX]]
// CHECK:   br [[EPILOG_BB]]
//
// CHECK: [[EPILOG_BB]]:
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s360________guardEnumyyAA08IndirectF0OyxGlF'
func s360________guardEnum<T>(_ e: IndirectEnum<T>) {
  do {
    guard case .Node(let x) = e else { return }
    _ = x
  }
}

// Tests contextual init() of opaque value types
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s370_____optToOptCastyxSgAClF : $@convention(thin) <T> (@in_guaranteed Optional<T>) -> @out Optional<T> {
// CHECK: bb0([[ARG:%.*]] : $Optional<T>):
// CHECK:   [[COPY__ARG:%.*]] = copy_value [[ARG]]
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return [[COPY__ARG]] : $Optional<T>
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s370_____optToOptCastyxSgAClF'
func s370_____optToOptCast<T>(_ x : T!) -> T? {
  return x
}

// Tests casting optional opaques to optional opaques
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s380___contextualInityySiSgF : $@convention(thin) (Optional<Int>) -> () {
// CHECK: bb0([[ARG:%.*]] : $Optional<Int>):
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var Optional<Int> }, var
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[ALLOC_OF_BOX]]
// CHECK:   store [[ARG]] to [trivial] [[PROJ_BOX]] : $*Optional<Int>
// CHECK:   destroy_value [[ALLOC_OF_BOX]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s380___contextualInityySiSgF'
func s380___contextualInit(_ a : Int?) {
  var x: Int! = a
  _ = x
}

// Tests opaque call result types
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s390___addrCallResultyyxycSglF : $@convention(thin) <T> (@guaranteed Optional<@callee_guaranteed () -> @out T>) -> () {
// CHECK: bb0([[ARG:%.*]] : $Optional<@callee_guaranteed () -> @out T>):
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <T>
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[ALLOC_OF_BOX]]
// CHECK:   [[COPY__ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[SENUM:%.*]] = select_enum [[COPY__ARG]]
// CHECK:   cond_br [[SENUM]], bb3, bb1
// CHECK: bb1:
// CHECK:   br bb2
// CHECK: bb2:
// CHECK:   [[ONONE:%.*]] = enum $Optional<T>, #Optional.none!enumelt
// CHECK:   br bb4([[ONONE]] : $Optional<T>)
// CHECK: bb4(%{{.*}} : $Optional<T>):
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s390___addrCallResultyyxycSglF'
func s390___addrCallResult<T>(_ f: (() -> T)?) {
  var x = f?()
  _ = x
}

// Tests reabstraction / partial apply of protocols under opaque value mode
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s400______maybeCloneP1cyAA8Clonable_p_tF : $@convention(thin) (@in_guaranteed Clonable) -> () {
// CHECK: bb0([[ARG:%.*]] : $Clonable):
// CHECK:   [[OPEN_ARG:%.*]] = open_existential_value [[ARG]] : $Clonable
// CHECK:   [[APPLY_OPAQUE:%.*]] = apply %{{.*}}<@opened({{.*}}) Clonable>([[OPEN_ARG]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed () -> @out Optional<τ_0_0>
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}<@opened({{.*}}) Clonable>([[APPLY_OPAQUE]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@guaranteed @callee_guaranteed () -> @out Optional<τ_0_0>) -> @out Optional<Clonable>
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s400______maybeCloneP1cyAA8Clonable_p_tF'
func s400______maybeCloneP(c: Clonable) {
  let _: () -> Clonable? = c.maybeClone
}

// Tests global opaque values / subscript rvalues
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s410__globalRvalueGetyS2iF : $@convention(thin) (Int) -> Int {
// CHECK: bb0([[ARG:%.*]] : $Int):
// CHECK:   [[GLOBAL_ADDR:%.*]] = global_addr @$S20opaque_values_silgen16subscriptableGetAA013SubscriptableE0_pvp : $*SubscriptableGet
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]] : $*SubscriptableGet
// CHECK:   [[OPEN_ARG:%.*]] = open_existential_addr immutable_access [[READ]] : $*SubscriptableGet to $*@opened
// CHECK:   [[GET_OPAQUE:%.*]] = load [copy] [[OPEN_ARG]] : $*@opened
// CHECK:   [[RETVAL:%.*]] = apply %{{.*}}<@opened({{.*}}) SubscriptableGet>([[ARG]], [[GET_OPAQUE]]) : $@convention(witness_method: SubscriptableGet) <τ_0_0 where τ_0_0 : SubscriptableGet> (Int, @in_guaranteed τ_0_0) -> Int
// CHECK:   destroy_value [[GET_OPAQUE]]
// CHECK:   return [[RETVAL]] : $Int
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s410__globalRvalueGetyS2iF'
func s410__globalRvalueGet(_ i : Int) -> Int {
  return subscriptableGet[i]
}

// Tests global opaque values / subscript lvalues
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s420__globalLvalueGetyS2iF : $@convention(thin) (Int) -> Int {
// CHECK: bb0([[ARG:%.*]] : $Int):
// CHECK:   [[GLOBAL_ADDR:%.*]] = global_addr @$S20opaque_values_silgen19subscriptableGetSetAA013SubscriptableeF0_pvp : $*SubscriptableGetSet
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]] : $*SubscriptableGetSet
// CHECK:   [[OPEN_ARG:%.*]] = open_existential_addr immutable_access [[READ]] : $*SubscriptableGetSet to $*@opened
// CHECK:   [[GET_OPAQUE:%.*]] = load [copy] [[OPEN_ARG]] : $*@opened
// CHECK:   [[RETVAL:%.*]] = apply %{{.*}}<@opened({{.*}}) SubscriptableGetSet>([[ARG]], [[GET_OPAQUE]]) : $@convention(witness_method: SubscriptableGetSet) <τ_0_0 where τ_0_0 : SubscriptableGetSet> (Int, @in_guaranteed τ_0_0) -> Int
// CHECK:   destroy_value [[GET_OPAQUE]]
// CHECK:   return [[RETVAL]] : $Int
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s420__globalLvalueGetyS2iF'
func s420__globalLvalueGet(_ i : Int) -> Int {
  return subscriptableGetSet[i]
}

// Tests tuple transformation
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s430_callUnreachableF1tyx_tlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[APPLY_T:%.*]] = apply %{{.*}}<((T) -> (), T)>() : $@convention(thin) <τ_0_0> () -> @out Optional<(Int, τ_0_0)>
// CHECK:   switch_enum [[APPLY_T]] : $Optional<(Int, (@callee_guaranteed (@in_guaranteed T) -> @out (), T))>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb2([[ENUMARG:%.*]] : $(Int, (@callee_guaranteed (@in_guaranteed T) -> @out (), T))):
// CHECK:   ([[TELEM0:%.*]], [[TELEM1:%.*]]) = destructure_tuple [[ENUMARG]] : $(Int, (@callee_guaranteed (@in_guaranteed T) -> @out (), T))
// CHECK:   ([[TELEM10:%.*]], [[TELEM11:%.*]]) = destructure_tuple [[TELEM1]] : $(@callee_guaranteed (@in_guaranteed T) -> @out (), T)
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}<T>([[TELEM10]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> @out ()) -> ()
// CHECK:   [[NEWT0:%.*]] = tuple ([[PAPPLY]] : $@callee_guaranteed (@in_guaranteed T) -> (), [[TELEM11]] : $T)
// CHECK:   [[NEWT1:%.*]] = tuple ([[TELEM0]] : $Int, [[NEWT0]] : $(@callee_guaranteed (@in_guaranteed T) -> (), T))
// CHECK:   [[NEWENUM:%.*]] = enum $Optional<(Int, (@callee_guaranteed (@in_guaranteed T) -> (), T))>, #Optional.some!enumelt.1, [[NEWT1]] : $(Int, (@callee_guaranteed (@in_guaranteed T) -> (), T))
// CHECK:   br bb3([[NEWENUM]] : $Optional<(Int, (@callee_guaranteed (@in_guaranteed T) -> (), T))>)
// CHECK: bb3([[ENUMIN:%.*]] : $Optional<(Int, (@callee_guaranteed (@in_guaranteed T) -> (), T))>):
// CHECK:   destroy_value [[ENUMIN]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s430_callUnreachableF1tyx_tlF'
func s430_callUnreachableF<T>(t: T) {
  let _: (Int, ((T) -> (), T))? = unreachableF()
}

// Further testing for conditional checked cast under opaque value mode - make sure we don't create a buffer for results
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s440__cleanupEmissionyyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   checked_cast_value_br [[COPY_ARG]] : $T to $EmptyP, bb2, bb1
//
// CHECK: bb2([[PTYPE:%.*]] : $EmptyP):
// CHECK:   [[PSOME:%.*]] = enum $Optional<EmptyP>, #Optional.some!enumelt.1, [[PTYPE]] : $EmptyP
// CHECK:   br bb3([[PSOME]] : $Optional<EmptyP>)
//
// CHECK: bb3([[ENUMRES:%.*]] : $Optional<EmptyP>):
// CHECK:   switch_enum [[ENUMRES]] : $Optional<EmptyP>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   br [[NONE_TRAMPOLINE:bb[0-9]+]]
//
// CHECK: [[NONE_TRAMPOLINE]]:
// CHECK:   br [[EPILOG_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]([[ENUMRES2:%.*]] : $EmptyP):
// CHECK:   br [[CONT_BB:bb[0-9]+]]
//
// CHECK: [[CONT_BB]]:
// CHECK:   destroy_value [[ENUMRES2]]
// CHECK:   br [[EPILOG_BB]]
//
// CHECK: [[EPILOG_BB]]:
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s440__cleanupEmissionyyxlF'
func s440__cleanupEmission<T>(_ x: T) {
  guard let x2 = x as? EmptyP else { return }
  _ = x2
}

// Test SILGenBuilder.loadCopy().
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s450__________lastValyxxd_tlF : $@convention(thin) <T> (@guaranteed Array<T>) -> @out T
// CHECK: [[LOAD:%.*]] = load [copy] %{{.*}} : $*T
// CHECK: return [[LOAD]] : $T
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s450__________lastValyxxd_tlF'
func s450__________lastVal<T>(_ rest: T...) -> T {
  var minValue: T
  for value in rest {
    minValue = value
  }
  return minValue
}

// Test SILGenFunction::emitPointerToPointer.
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s460______________foo1pSRyxGSPyxG_tlF : $@convention(thin) <Element> (UnsafePointer<Element>) -> UnsafeBufferPointer<Element> {
// CHECK: [[F:%.*]] = function_ref @$Ss017_convertPointerToB8Argumentyq_xs01_B0RzsABR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Pointer, τ_0_1 : _Pointer> (@in_guaranteed τ_0_0) -> @out τ_0_1
// CHECK: apply [[F]]<UnsafePointer<Element>, UnsafePointer<Element>>(%0) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Pointer, τ_0_1 : _Pointer> (@in_guaranteed τ_0_0) -> @out τ_0_1
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s460______________foo1pSRyxGSPyxG_tlF'
func s460______________foo<Element>(p: UnsafePointer<Element>) -> UnsafeBufferPointer<Element> {
  return UnsafeBufferPointer(start: p, count: 1)
}

// Test emitNativeToCBridgedNonoptionalValue.
// ---
// CHECK-objc-LABEL: sil hidden @$S20opaque_values_silgen21s470________nativeToC7fromAnyyXlyp_tF : $@convention(thin) (@in_guaranteed Any) -> @owned AnyObject {
// CHECK-objc bb0(%0 : $Any):
// CHECK-objc [[BORROW:%.*]] = begin_borrow %0 : $Any
// CHECK-objc [[SRC:%.*]] = copy_value [[BORROW]] : $Any
// CHECK-objc [[OPEN:%.*]] = open_existential_opaque [[SRC]] : $Any to $@opened
// CHECK-objc [[COPY:%.*]] = copy_value [[OPEN]] : $@opened
// CHECK-objc [[F:%.*]] = function_ref @$Ss27_bridgeAnythingToObjectiveCyyXlxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned AnyObject
// CHECK-objc [[RET:%.*]] = apply [[F]]<@opened("{{.*}}") Any>([[COPY]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned AnyObject
// CHECK-objc destroy_value [[SRC]] : $Any
// CHECK-objc destroy_value %0 : $Any
// CHECK-objc return [[RET]] : $AnyObject
// CHECK-objc-LABEL: } // end sil function '$S20opaque_values_silgen21s470________nativeToC7fromAnyyXlyp_tF'
#if _runtime(_ObjC)
func s470________nativeToC(fromAny any: Any) -> AnyObject {
  return any as AnyObject
}
#endif

// Test emitOpenExistential.
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s480_________getError04someF0yps0F0_p_tF : $@convention(thin) (@guaranteed Error) -> @out Any {
// CHECK: bb0([[ARG:%.*]] : $Error):
// CHECK: [[VAL:%.*]] = open_existential_box_value [[ARG]] : $Error to $@opened("{{.*}}") Error
// CHECK: [[COPY:%.*]] = copy_value [[VAL]] : $@opened("{{.*}}") Error
// CHECK: [[ANY:%.*]] = init_existential_value [[COPY]] : $@opened("{{.*}}") Error, $@opened("{{.*}}") Error, $Any
// CHECK-NOT: destroy_value [[ARG]] : $Error
// CHECK: return [[ANY]] : $Any
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s480_________getError04someF0yps0F0_p_tF'
func s480_________getError(someError: Error) -> Any {
  return someError
}

// Test SILBuilder.createLoadBorrow.
// ---
// CHECK-LABEL: sil private @$S20opaque_values_silgen21s490_______loadBorrowyyF3FooL_V3foo3pos7ElementQzSg5IndexQz_tF : $@convention(method) <Elements where Elements : Collection> (@in_guaranteed Elements.Index, @inout Foo<Elements>) -> @out Optional<Elements.Element> {
// CHECK: bb0([[ARG0:%.*]] : $Elements.Index, [[ARG1:%.*]] : $*Foo<Elements>):
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[ARG1]] : $*Foo<Elements>
// CHECK: [[LOAD:%.*]] = load [copy] [[READ]] : $*Foo<Elements>
// CHECK: end_access [[READ]] : $*Foo<Elements>
// CHECK: [[BORROW_LOAD:%.*]] = begin_borrow [[LOAD]]
// CHECK: [[EXTRACT:%.*]] = struct_extract [[BORROW_LOAD]] : $Foo<Elements>, #<abstract function>Foo._elements
// CHECK: [[COPYELT:%.*]] = copy_value [[EXTRACT]] : $Elements
// CHECK: [[COPYIDX:%.*]] = copy_value [[ARG0]] : $Elements.Index
// CHECK: [[WT:%.*]] = witness_method $Elements, #Collection.subscript!getter.1 : <Self where Self : Collection> (Self) -> (Self.Index) -> Self.Element : $@convention(witness_method: Collection) <τ_0_0 where τ_0_0 : Collection> (@in_guaranteed τ_0_0.Index, @in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK: [[RESULT:%.*]] = apply [[WT]]<Elements>([[COPYIDX]], [[COPYELT]]) : $@convention(witness_method: Collection) <τ_0_0 where τ_0_0 : Collection> (@in_guaranteed τ_0_0.Index, @in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK: destroy_value [[COPYELT]] : $Elements
// CHECK: [[ENUM_RESULT:%.*]] = enum $Optional<Elements.Element>, #Optional.some!enumelt.1, [[RESULT]] : $Elements.Element
// CHECK: destroy_value [[LOAD]]
// CHECK-NOT: destroy_value [[ARG0]] : $Elements.Index
// CHECK: return [[ENUM_RESULT]] : $Optional<Elements.Element>
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s490_______loadBorrowyyF3FooL_V3foo3pos7ElementQzSg5IndexQz_tF'

func s490_______loadBorrow() {
  struct Foo<Elements : Collection> {
    internal let _elements: Elements

    public mutating func foo(pos: Elements.Index) -> Elements.Element? {
      return _elements[pos]
    }
  }
  var foo = Foo(_elements: [])
  _ = foo.foo(pos: 1)
}

protocol ConvertibleToP {
  func asP() -> P
}

// Test visitBindOptionalExpr
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s500_______getAnyHashyAA1P_pSgAA14ConvertibleToP_pSgF : $@convention(thin) (@in_guaranteed Optional<ConvertibleToP>) -> @out Optional<P> {
// CHECK: bb0(%0 : $Optional<ConvertibleToP>):
// CHECK: [[COPY:%.*]] = copy_value [[ARG]] : $Optional<ConvertibleToP>
// CHECK: [[DATA:%.*]] = unchecked_enum_data [[COPY]] : $Optional<ConvertibleToP>, #Optional.some!enumelt.1
// CHECK: [[BORROW_DATA:%.*]] = begin_borrow [[DATA]] : $ConvertibleToP
// CHECK: [[VAL:%.*]] = open_existential_value [[BORROW_DATA]] : $ConvertibleToP to $@opened("{{.*}}") ConvertibleToP
// CHECK: [[WT:%.*]] = witness_method $@opened("{{.*}}") ConvertibleToP, #ConvertibleToP.asP!1 : <Self where Self : ConvertibleToP> (Self) -> () -> P, [[VAL]] : $@opened("{{.*}}") ConvertibleToP : $@convention(witness_method: ConvertibleToP) <τ_0_0 where τ_0_0 : ConvertibleToP> (@in_guaranteed τ_0_0) -> @out P
// CHECK: [[AS_P:%.*]] = apply [[WT]]<@opened("{{.*}}") ConvertibleToP>([[VAL]]) : $@convention(witness_method: ConvertibleToP) <τ_0_0 where τ_0_0 : ConvertibleToP> (@in_guaranteed τ_0_0) -> @out P
// CHECK: [[ENUM:%.*]] = enum $Optional<P>, #Optional.some!enumelt.1, [[AS_P]] : $P
// CHECK: destroy_value [[DATA]] : $ConvertibleToP
// CHECK: br bb{{.*}}([[ENUM]] : $Optional<P>)
// CHECK: // end sil function '$S20opaque_values_silgen21s500_______getAnyHashyAA1P_pSgAA14ConvertibleToP_pSgF'
func s500_______getAnyHash(_ value: ConvertibleToP?) -> P? {
  return value?.asP()
}

public protocol FooP {
  func foo() -> Self
}

// Test emitting a protocol witness for a method (with @in_guaranteed self) on a dependent generic type.
// ---
// CHECK-LABEL: sil private [transparent] [thunk] @$S20opaque_values_silgen21s510_______OpaqueSelfVyxGAA4FooPA2aEP3fooxyFTW : $@convention(witness_method: FooP) <τ_0_0> (@in_guaranteed s510_______OpaqueSelf<τ_0_0>) -> @out s510_______OpaqueSelf<τ_0_0> {
// CHECK: bb0(%0 : $s510_______OpaqueSelf<τ_0_0>):
// CHECK:   [[FN:%.*]] = function_ref @$S20opaque_values_silgen21s510_______OpaqueSelfV3fooACyxGyF : $@convention(method) <τ_0_0> (@in_guaranteed s510_______OpaqueSelf<τ_0_0>) -> @out s510_______OpaqueSelf<τ_0_0>
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<τ_0_0>(%0) : $@convention(method) <τ_0_0> (@in_guaranteed s510_______OpaqueSelf<τ_0_0>) -> @out s510_______OpaqueSelf<τ_0_0>
// CHECK:   return [[RESULT]] : $s510_______OpaqueSelf<τ_0_0>
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s510_______OpaqueSelfVyxGAA4FooPA2aEP3fooxyFTW'
struct s510_______OpaqueSelf<Base> : FooP {
  var x: Base

  func foo() -> s510_______OpaqueSelf<Base> {
    return self
  }
}

// Tests conditional value casts and correspondingly generated reabstraction thunk, with <T> types
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen21s999_____condTFromAnyyyyp_xtlF : $@convention(thin) <T> (@in_guaranteed Any, @in_guaranteed T) -> () {
// CHECK: bb0([[ARG0:%.*]] : $Any, [[ARG1:%.*]] : $T):
// CHECK:   [[COPY__ARG:%.*]] = copy_value [[ARG]]
// CHECK:   checked_cast_value_br [[COPY__ARG]] : $Any to $@callee_guaranteed (@in_guaranteed (Int, T)) -> @out (Int, T), bb2, bb1
// CHECK: bb2([[THUNK_PARAM:%.*]] : $@callee_guaranteed (@in_guaranteed (Int, T)) -> @out (Int, T)):
// CHECK:   [[THUNK_REF:%.*]] = function_ref @{{.*}} : $@convention(thin) <τ_0_0> (Int, @in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed (Int, τ_0_0)) -> @out (Int, τ_0_0)) -> (Int, @out τ_0_0)
// CHECK:   partial_apply [callee_guaranteed] [[THUNK_REF]]<T>([[THUNK_PARAM]])
// CHECK: bb6:
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen21s999_____condTFromAnyyyyp_xtlF'
func s999_____condTFromAny<T>(_ x: Any, _ y: T) {
  if let f = x as? (Int, T) -> (Int, T) {
    _ = f(42, y)
  }
}

// Make sure that we insert a destroy of the box even though we used an Int type.
// CHECK-LABEL: sil @$S20opaque_values_silgen22s020_______assignToVaryyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[Y_BOX:%.*]] = alloc_box ${ var Int }, var, name "y"
// CHECK:   [[PROJECT_Y_BOX:%.*]] = project_box [[Y_BOX]] : ${ var Int }, 0
// CHECK:   [[X_BOX:%.*]] = alloc_box ${ var Any }, var, name "x"
// CHECK:   [[PROJECT_X_BOX:%.*]] = project_box [[X_BOX]] : ${ var Any }, 0
// CHECK:   [[ACCESS_PROJECT_Y_BOX:%.*]] = begin_access [read] [unknown] [[PROJECT_Y_BOX]] : $*Int
// CHECK:   [[Y:%.*]] = load [trivial] [[ACCESS_PROJECT_Y_BOX]] : $*Int
// CHECK:   [[Y_ANY_FOR_X:%.*]] = init_existential_value [[Y]] : $Int, $Int, $Any
// CHECK:   store [[Y_ANY_FOR_X]] to [init] [[PROJECT_X_BOX]]
// CHECK:   [[ACCESS_PROJECT_Y_BOX:%.*]] = begin_access [read] [unknown] [[PROJECT_Y_BOX]] : $*Int
// CHECK:   [[Y:%.*]] = load [trivial] [[ACCESS_PROJECT_Y_BOX]] : $*Int
// CHECK:   [[Y_ANY_FOR_Z:%.*]] = init_existential_value [[Y]] : $Int, $Int, $Any
// CHECK:   destroy_value [[Y_ANY_FOR_Z]]
// CEHCK:   destroy_value [[X_BOX]]
// CHECK:   destroy_value [[Y_BOX]]
// CHECK: } // end sil function '$S20opaque_values_silgen22s020_______assignToVaryyF'
public func s020_______assignToVar() {
  var y: Int = 3
  var x: Any = y
  let z: Any = y
}

// s250_________testBoxT continued Test Implicit Value Construction under Opaque value mode
// ---
// CHECK-LABEL: sil hidden @$S20opaque_values_silgen3BoxV1tACyxGx_tcfC : $@convention(method) <T> (@in T, @thin Box<T>.Type) -> @out Box<T> {
// CHECK: bb0([[ARG0:%.*]] : $T, [[ARG1:%.*]] : $@thin Box<T>.Type):
// CHECK:   [[RETVAL:%.*]] = struct $Box<T> ([[ARG0]] : $T)
// CHECK:   return [[RETVAL]] : $Box<T>
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen3BoxV1tACyxGx_tcfC'

// s270_convOptAnyStruct continued Test: reabstraction thunk helper
// ---
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$S20opaque_values_silgen9AnyStructVSgACIegnr_A2DIegnr_TR : $@convention(thin) (@in_guaranteed Optional<AnyStruct>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct) -> @out Optional<AnyStruct> {
// CHECK: bb0([[ARG0:%.*]] : $Optional<AnyStruct>, [[ARG1:%.*]] : $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct):
// CHECK:   [[APPLYARG:%.*]] = apply [[ARG1]]([[ARG0]]) : $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct
// CHECK:   [[RETVAL:%.*]] = enum $Optional<AnyStruct>, #Optional.some!enumelt.1, [[APPLYARG]] : $AnyStruct
// CHECK:   return [[RETVAL]] : $Optional<AnyStruct>
// CHECK-LABEL: } // end sil function '$S20opaque_values_silgen9AnyStructVSgACIegnr_A2DIegnr_TR'

// s300__convETupleToAny continued Test: reabstraction of () to Any
// ---
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SIeg_ypIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @out Any {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed () -> ()):
// CHECK:   [[ASTACK:%.*]] = alloc_stack $Any
// CHECK:   [[IADDR:%.*]] = init_existential_addr [[ASTACK]] : $*Any, $()
// CHECK:   [[APPLYARG:%.*]] = apply [[ARG]]() : $@callee_guaranteed () -> ()
// CHECK:   [[LOAD_EXIST:%.*]] = load [trivial] [[IADDR]] : $*()
// CHECK:   [[RETVAL:%.*]] = init_existential_value [[LOAD_EXIST]] : $(), $(), $Any
// CHECK:   return [[RETVAL]] : $Any
// CHECK-LABEL: } // end sil function '$SIeg_ypIegr_TR'

// s310_convIntTupleAny continued Test: reabstraction of non-empty tuple to Any
// ---
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SS2iIegdd_ypIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> (Int, Int)) -> @out Any {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed () -> (Int, Int)):
// CHECK:   [[ASTACK:%.*]] = alloc_stack $Any
// CHECK:   [[IADDR:%.*]] = init_existential_addr [[ASTACK]] : $*Any, $(Int, Int)
// CHECK:   [[TADDR0:%.*]] = tuple_element_addr [[IADDR]] : $*(Int, Int), 0
// CHECK:   [[TADDR1:%.*]] = tuple_element_addr [[IADDR]] : $*(Int, Int), 1
// CHECK:   [[APPLYARG:%.*]] = apply [[ARG]]() : $@callee_guaranteed () -> (Int, Int)
// CHECK:   [[TEXTRACT0:%.*]] = tuple_extract [[APPLYARG]] : $(Int, Int), 0
// CHECK:   [[TEXTRACT1:%.*]] = tuple_extract [[APPLYARG]] : $(Int, Int), 1
// CHECK:   store [[TEXTRACT0]] to [trivial] [[TADDR0]] : $*Int
// CHECK:   store [[TEXTRACT1]] to [trivial] [[TADDR1]] : $*Int
// CHECK:   [[LOAD_EXIST:%.*]] = load [trivial] [[IADDR]] : $*(Int, Int)
// CHECK:   [[RETVAL:%.*]] = init_existential_value [[LOAD_EXIST]] : $(Int, Int), $(Int, Int), $Any
// CHECK:   dealloc_stack [[ASTACK]] : $*Any
// CHECK:   return [[RETVAL]] : $Any
// CHECK-LABEL: } // end sil function '$SS2iIegdd_ypIegr_TR'


// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @{{.*}} : $@convention(thin) (Int, Int, Int, Int, Int, @guaranteed @callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int)) -> (Int, Int, Int, Int, Int)
// CHECK: bb0([[ARG0:%.*]] : $Int, [[ARG1:%.*]] : $Int, [[ARG2:%.*]] : $Int, [[ARG3:%.*]] : $Int, [[ARG4:%.*]] : $Int, [[ARG5:%.*]] : $@callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int)):
// CHECK:   [[TUPLE_TO_APPLY0:%.*]] = tuple ([[ARG2]] : $Int, [[ARG3]] : $Int)
// CHECK:   [[TUPLE_TO_APPLY1:%.*]] = tuple ([[ARG1]] : $Int, [[TUPLE_TO_APPLY0]] : $(Int, Int))
// CHECK:   [[TUPLE_TO_APPLY2:%.*]] = tuple ([[ARG0]] : $Int, [[TUPLE_TO_APPLY1]] : $(Int, (Int, Int)), [[ARG4]] : $Int)
// CHECK:   [[TUPLE_APPLY:%.*]] = apply [[ARG5]]([[TUPLE_TO_APPLY2]]) : $@callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int)
// CHECK:   [[RET_VAL0:%.*]] = tuple_extract [[TUPLE_APPLY]] : $(Int, (Int, (Int, Int)), Int), 0
// CHECK:   [[TUPLE_EXTRACT1:%.*]] = tuple_extract [[TUPLE_APPLY]] : $(Int, (Int, (Int, Int)), Int), 1
// CHECK:   [[RET_VAL1:%.*]] = tuple_extract [[TUPLE_EXTRACT1]] : $(Int, (Int, Int)), 0
// CHECK:   [[TUPLE_EXTRACT2:%.*]] = tuple_extract [[TUPLE_EXTRACT1]] : $(Int, (Int, Int)), 1
// CHECK:   [[RET_VAL2:%.*]] = tuple_extract [[TUPLE_EXTRACT2]]  : $(Int, Int), 0
// CHECK:   [[RET_VAL3:%.*]] = tuple_extract [[TUPLE_EXTRACT2]]  : $(Int, Int), 1
// CHECK:   [[RET_VAL4:%.*]] = tuple_extract [[TUPLE_APPLY]] : $(Int, (Int, (Int, Int)), Int), 2
// CHECK:   [[RET_VAL_TUPLE:%.*]] = tuple ([[RET_VAL0]] : $Int, [[RET_VAL1]] : $Int, [[RET_VAL2]] : $Int, [[RET_VAL3]] : $Int, [[RET_VAL4]] : $Int)
// CHECK:   return [[RET_VAL_TUPLE]] : $(Int, Int, Int, Int, Int)
// CHECK-LABEL: } // end sil function '{{.*}}'

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @{{.*}} : $@convention(thin) <T> (Int, @in_guaranteed T, @guaranteed @callee_guaranteed (@in_guaranteed (Int, T)) -> @out (Int, T)) -> (Int, @out T) {
// CHECK: bb0([[ARG0:%.*]] : $Int, [[ARG1:%.*]] : $T, [[ARG2:%.*]] : $@callee_guaranteed (@in_guaranteed (Int, T)) -> @out (Int, T)):
// CHECK:   [[TUPLE_TO_APPLY:%.*]] = tuple ([[ARG0]] : $Int, [[ARG1]] : $T)
// CHECK:   [[TUPLE_APPLY:%.*]] = apply [[ARG2]]([[TUPLE_TO_APPLY]]) : $@callee_guaranteed (@in_guaranteed (Int, T)) -> @out (Int, T)
// CHECK:   [[TUPLE_BORROW:%.*]] = begin_borrow [[TUPLE_APPLY]] : $(Int, T)
// CHECK:   [[RET_VAL0:%.*]] = tuple_extract [[TUPLE_BORROW]] : $(Int, T), 0
// CHECK:   [[TUPLE_EXTRACT:%.*]] = tuple_extract [[TUPLE_BORROW]] : $(Int, T), 1
// CHECK:   [[RET_VAL1:%.*]] = copy_value [[TUPLE_EXTRACT]] : $T
// CHECK:   end_borrow [[TUPLE_BORROW]] from [[TUPLE_APPLY]] : $(Int, T), $(Int, T)
// CHECK:   destroy_value [[TUPLE_APPLY]] : $(Int, T)
// CHECK:   [[RET_VAL_TUPLE:%.*]] = tuple ([[RET_VAL0]] : $Int, [[RET_VAL1]] : $T)
// CHECK:   return [[RET_VAL_TUPLE]] : $(Int, T)
// CHECK-LABEL: } // end sil function '{{.*}}'


// Tests LogicalPathComponent's writeback for opaque value types
// ---
// CHECK-LABEL: sil @$Ss10DictionaryV20opaque_values_silgenE22inoutAccessOfSubscript3keyyq__tF : $@convention(method) <Key, Value where Key : Hashable> (@in_guaranteed Value, @inout Dictionary<Key, Value>) -> () {
// CHECK: bb0([[ARG0:%.*]] : $Value, [[ARG1:%.*]] : $*Dictionary<Key, Value>):
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[ARG1]] : $*Dictionary<Key, Value>
// CHECK:   [[OPTIONAL_ALLOC:%.*]] = alloc_stack $Optional<Value>
// CHECK:   switch_enum_addr [[OPTIONAL_ALLOC]] : $*Optional<Value>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb2:
// CHECK:   [[OPTIONAL_LOAD:%.*]] = load [take] [[OPTIONAL_ALLOC]] : $*Optional<Value>
// CHECK:   apply {{.*}}<Key, Value>([[OPTIONAL_LOAD]], {{.*}}, [[WRITE]]) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@in Optional<τ_0_1>, @in τ_0_1, @inout Dictionary<τ_0_0, τ_0_1>) -> ()
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$Ss10DictionaryV20opaque_values_silgenE22inoutAccessOfSubscript3keyyq__tF'

// Tests materializeForSet's createSetterCallback for opaque values
// ---
// CHECK-LABEL: sil shared [transparent] [serialized] @$Ss10DictionaryV20opaque_values_silgenEyq_Sgq_cimytfU_ : $@convention(method) <Key, Value where Key : Hashable> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Dictionary<Key, Value>, @thick Dictionary<Key, Value>.Type) -> () {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.RawPointer, [[ARG1:%.*]] : $*Builtin.UnsafeValueBuffer, [[ARG2:%.*]] : $*Dictionary<Key, Value>, [[ARG3:%.*]] : $@thick Dictionary<Key, Value>.Type):
// CHECK:   [[PROJ_VAL1:%.*]] = project_value_buffer $Value in [[ARG1]] : $*Builtin.UnsafeValueBuffer
// CHECK:   [[LOAD_VAL1:%.*]] = load [take] [[PROJ_VAL1]] : $*Value
// CHECK:   [[ADDR_VAL0:%.*]] = pointer_to_address [[ARG0]] : $Builtin.RawPointer to [strict] $*Optional<Value>
// CHECK:   [[LOAD_VAL0:%.*]] = load [take] [[ADDR_VAL0]] : $*Optional<Value>
// CHECK:   apply {{.*}}<Key, Value>([[LOAD_VAL0]], [[LOAD_VAL1]], [[ARG2]]) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@in Optional<τ_0_1>, @in τ_0_1, @inout Dictionary<τ_0_0, τ_0_1>) -> ()
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$Ss10DictionaryV20opaque_values_silgenEyq_Sgq_cimytfU_'
extension Dictionary {
  public subscript(key: Value) -> Value? {
    @inline(__always)
    get {
      return key
    }
    set(newValue) {
    }
  }
  
  public mutating func inoutAccessOfSubscript(key: Value) {
    func increment(x: inout Value) { }

    increment(x: &self[key]!)
  }
}

// s400______maybeCloneP continued Test: reabstraction thunk
// ---
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SxSgIegr_20opaque_values_silgen8Clonable_pSgIegr_AbCRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@guaranteed @callee_guaranteed () -> @out Optional<τ_0_0>) -> @out Optional<Clonable> {
// CHECK: bb0([[ARG:%.*]] : $@callee_guaranteed () -> @out Optional<τ_0_0>):
// CHECK:   [[APPLY_ARG:%.*]] = apply [[ARG]]() : $@callee_guaranteed () -> @out Optional<τ_0_0>
// CHECK:   switch_enum [[APPLY_ARG]] : $Optional<τ_0_0>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb1:
// CHECK:   [[ONONE:%.*]] = enum $Optional<Clonable>, #Optional.none!enumelt
// CHECK:   br bb3([[ONONE]] : $Optional<Clonable>)
// CHECK: bb2([[ENUM_SOME:%.*]] : $τ_0_0):
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[ENUM_SOME]] : $τ_0_0, $τ_0_0, $Clonable
// CHECK:   [[OSOME:%.*]] = enum $Optional<Clonable>, #Optional.some!enumelt.1, [[INIT_OPAQUE]] : $Clonable
// CHECK:   br bb3([[OSOME]] : $Optional<Clonable>)
// CHECK: bb3([[RETVAL:%.*]] : $Optional<Clonable>):
// CHECK:   return [[RETVAL]] : $Optional<Clonable>
// CHECK-LABEL: } // end sil function '$SxSgIegr_20opaque_values_silgen8Clonable_pSgIegr_AbCRzlTR'

// s320__transImplodeAny continued Test: reabstraction thunk
// ---
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SypIegn_S2iIegyy_TR : $@convention(thin) (Int, Int, @guaranteed @callee_guaranteed (@in_guaranteed Any) -> ()) -> () {
// CHECK: bb0([[ARG0:%.*]] : $Int, [[ARG1:%.*]] : $Int, [[ARG2:%.*]] : $@callee_guaranteed (@in_guaranteed Any) -> ()):
// CHECK:   [[ASTACK:%.*]] = alloc_stack $Any
// CHECK:   [[IADDR:%.*]] = init_existential_addr [[ASTACK]] : $*Any, $(Int, Int)
// CHECK:   [[TADDR0:%.*]] = tuple_element_addr [[IADDR]] : $*(Int, Int), 0
// CHECK:   store [[ARG0]] to [trivial] [[TADDR0]] : $*Int
// CHECK:   [[TADDR1:%.*]] = tuple_element_addr [[IADDR]] : $*(Int, Int), 1
// CHECK:   store [[ARG1]] to [trivial] [[TADDR1]] : $*Int
// CHECK:   [[LOAD_EXIST:%.*]] = load [trivial] [[IADDR]] : $*(Int, Int)
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[LOAD_EXIST]] : $(Int, Int), $(Int, Int), $Any
// CHECK:   [[BORROWED_INIT_OPAQUE:%.*]] = begin_borrow [[INIT_OPAQUE]]
// CHECK:   [[APPLYARG:%.*]] = apply [[ARG2]]([[BORROWED_INIT_OPAQUE]]) : $@callee_guaranteed (@in_guaranteed Any) -> ()
// CHECK:   dealloc_stack [[ASTACK]] : $*Any
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$SypIegn_S2iIegyy_TR'
