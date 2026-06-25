// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-sil-opaque-values -Xllvm -sil-full-demangle -parse-stdlib -parse-as-library -module-name Swift %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

// Test SILGen -enable-sil-opaque-values

typealias AnyObject = Builtin.AnyObject

public enum Optional<T> {
  case none
  case some(T)
}

public protocol ExpressibleByNilLiteral {
  init(nilLiteral: ())
}

extension Optional : ExpressibleByNilLiteral {
  public init(nilLiteral: ()) {
    self = .none
  }
}

func _diagnoseUnexpectedNilOptional(_filenameStart: Builtin.RawPointer,
                                    _filenameLength: Builtin.Word,
                                    _filenameIsASCII: Builtin.Int1,
                                    _line: Builtin.Word,
                                    _isImplicitUnwrap: Builtin.Int1) {
  // This would usually contain an assert, but we don't need one since we are
  // just emitting SILGen.
}

precedencegroup AssignmentPrecedence { assignment: true }
precedencegroup CastingPrecedence {}
precedencegroup ComparisonPrecedence {}
precedencegroup TernaryPrecedence {}

public protocol Error {}

public protocol _ObjectiveCBridgeable {}

protocol EmptyP {}

struct AddressOnlyStruct : EmptyP {}

struct String { var ptr: Builtin.NativeObject }

public typealias _MaxBuiltinIntegerType = Builtin.IntLiteral

public protocol _ExpressibleByBuiltinIntegerLiteral {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}

public protocol ExpressibleByIntegerLiteral {
  associatedtype IntegerLiteralType : _ExpressibleByBuiltinIntegerLiteral

  init(integerLiteral value: IntegerLiteralType)
}

extension ExpressibleByIntegerLiteral
  where Self : _ExpressibleByBuiltinIntegerLiteral {
  @_transparent
  public init(integerLiteral value: Self) {
    self = value
  }
}

public protocol ExpressibleByFloatLiteral {}

typealias Bool = Builtin.Int1

public struct Int64 : ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  public var _value: Builtin.Int64
  public init(_builtinIntegerLiteral x: _MaxBuiltinIntegerType) {
    _value = Builtin.s_to_s_checked_trunc_IntLiteral_Int64(x).0
  }
  public typealias IntegerLiteralType = Int64
  public init(integerLiteral value: Int64) {
    self = value
  }
}

public protocol UnkeyedDecodingContainer {
  var isAtEnd: Builtin.Int1 { get }
}

public protocol Decoder {
  func unkeyedContainer() throws -> UnkeyedDecodingContainer
}

protocol FooP {
  func foo()
}

struct AnyStruct {
  let a: Any
}

protocol P {
  var x : Builtin.Int64 { get }
}

protocol P2 : P {}

struct TrivialStruct {
  var x: Builtin.Int64
}

extension TrivialStruct : P2 {}

protocol Clonable {
  func maybeClone() -> Self?
}

func unreachableF<T>() -> (Builtin.Int64, T)? { /* no body */ }

protocol ConvertibleToP {
  func asP() -> P
}

indirect enum IndirectEnum<T> {
  case Nil
  case Node(T)
}

protocol SubscriptableGet {
  subscript(a : Builtin.Int64) -> Builtin.Int64 { get }
}

protocol SubscriptableGetSet {
  subscript(a : Builtin.Int64) -> Builtin.Int64 { get set }
}

var subscriptableGet : SubscriptableGet?
var subscriptableGetSet : SubscriptableGetSet?

func genericInout<T>(_: inout T) {}

// =============================================================================
//                         Begin Test Cases
// =============================================================================

enum PAndSEnum { case A(EmptyP, String) }

// Tests Empty protocol + Builtin.NativeObject enum (including opaque tuples as a return value)
// ---
// Swift.f010_PAndS_cases() -> ()
// CHECK-LABEL: sil hidden [ossa] @$ss16f010_PAndS_casesyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[MTYPE:%.*]] = metatype $@thin PAndSEnum.Type
// CHECK:   [[EAPPLY:%.*]] = apply {{.*}}([[MTYPE]]) : $@convention(thin) (@thin PAndSEnum.Type) -> @owned @callee_guaranteed (@in_guaranteed any EmptyP, @guaranteed String) -> @out PAndSEnum
// CHECK:   destroy_value [[EAPPLY]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss16f010_PAndS_casesyyF'
func f010_PAndS_cases() {
  _ = PAndSEnum.A
}

// Init of Empty protocol + Builtin.NativeObject enum (including opaque tuples as a return value)
// ---
// implicit closure #2 (Swift.EmptyP, Swift.String) -> Swift.PAndSEnum in implicit closure #1 (Swift.PAndSEnum.Type) -> (Swift.EmptyP, Swift.String) -> Swift.PAndSEnum in Swift.f010_PAndS_cases() -> ()
// CHECK-LABEL: sil private [ossa] @$ss16f010_PAndS_casesyyFs0B5SEnumOs6EmptyP_p_SStcACmcfu_ACsAD_p_SStcfu0_ : $@convention(thin) (@in_guaranteed any EmptyP, @guaranteed String, @thin PAndSEnum.Type) -> @out PAndSEnum {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $any EmptyP, [[ARG1:%.*]] : @guaranteed $String, [[ARG2:%.*]] : @closureCapture $@thin PAndSEnum.Type):
// CHECK:   [[COPY0:%.*]] = copy_value [[ARG0]]
// CHECK:   [[COPY1:%.*]] = copy_value [[ARG1]]
// CHECK:   [[RTUPLE:%.*]] = tuple ([[COPY0]] : $any EmptyP, [[COPY1]] : $String)
// CHECK:   [[RETVAL:%.*]] = enum $PAndSEnum, #PAndSEnum.A!enumelt, [[RTUPLE]] : $(any EmptyP, String)
// CHECK:   return [[RETVAL]] : $PAndSEnum
// CHECK-LABEL: } // end sil function '$ss16f010_PAndS_casesyyFs0B5SEnumOs6EmptyP_p_SStcACmcfu_ACsAD_p_SStcfu0_'

// Test emitBuiltinReinterpretCast.
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss12f020_bitCast_2toq_x_q_mtr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @thick U.Type) -> @out U {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T,
// CHECK: [[COPY:%.*]] = copy_value [[ARG]] : $T
// CHECK: [[CAST:%.*]] = unchecked_bitwise_cast [[COPY]] : $T to $U
// CHECK: [[RET:%.*]] = copy_value [[CAST]] : $U
// CHECK: destroy_value [[COPY]] : $T
// CHECK: return [[RET]] : $U
// CHECK-LABEL: } // end sil function '$ss12f020_bitCast_2toq_x_q_mtr0_lF'
func f020_bitCast<T, U>(_ x: T, to type: U.Type) -> U {
  return Builtin.reinterpretCast(x)
}

// Test emitBuiltinCastReference
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss12f021_refCast_2toq_x_q_mtr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @thick U.Type) -> @out U {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T, %1 : $@thick U.Type):
// CHECK: [[COPY:%.*]] = copy_value [[ARG]] : $T
// CHECK: [[SRC:%.*]] = alloc_stack $T
// CHECK: store [[COPY]] to [init] [[SRC]] : $*T
// CHECK: [[DEST:%.*]] = alloc_stack $U
// CHECK: unchecked_ref_cast_addr  T in [[SRC]] : $*T to U in [[DEST]] : $*U
// CHECK: [[LOAD:%.*]] = load [take] [[DEST]] : $*U
// CHECK: dealloc_stack [[DEST]] : $*U
// CHECK: dealloc_stack [[SRC]] : $*T
// CHECK-NOT: destroy_value [[ARG]] : $T
// CHECK: return [[LOAD]] : $U
// CHECK-LABEL: } // end sil function '$ss12f021_refCast_2toq_x_q_mtr0_lF'
func f021_refCast<T, U>(_ x: T, to: U.Type) -> U {
  return Builtin.castReference(x)
}

// Test unsafe_bitwise_cast nontrivial ownership.
// ---
// CHECK-LABEL: sil [ossa] @$ss18f022_unsafeBitCast_2toq_x_q_mtr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @thick U.Type) -> @out U {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $T, [[ARG1:%.*]] : $@thick U.Type):
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG0]] : $T
// CHECK:   [[RESULT:%.*]] = unchecked_bitwise_cast [[ARG_COPY]] : $T to $U
// CHECK:   [[RESULT_COPY:%.*]] = copy_value [[RESULT]] : $U
// CHECK:   destroy_value [[ARG_COPY]] : $T
// CHECK:   return [[RESULT_COPY]] : $U
// CHECK-LABEL: } // end sil function '$ss18f022_unsafeBitCast_2toq_x_q_mtr0_lF'
public func f022_unsafeBitCast<T, U>(_ x: T, to type: U.Type) -> U {
  return Builtin.reinterpretCast(x)
}

// Test emitSemanticStore.
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss16f030_assigninoutyyxz_xtlF : $@convention(thin) <T> (@inout T, @in_guaranteed T) -> () {
// CHECK: bb0([[ARG0:%.*]] : $*T, [[ARG1:%.*]] : @guaranteed $T):
// CHECK:   [[CPY:%.*]] = copy_value [[ARG1]] : $T
// CHECK:   [[READ:%.*]] = begin_access [modify] [unknown] [[ARG0]] : $*T
// CHECK:   assign [[CPY]] to [[READ]] : $*T
// CHECK-NOT:   destroy_value [[ARG1]] : $T
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss16f030_assigninoutyyxz_xtlF'
func f030_assigninout<T>(_ a: inout T, _ b: T) {
  a = b
}

// Test that we no longer use copy_addr or tuple_element_addr when copy by value is possible
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss19f040_tupleReturnIntyBi64_Bi64__xt_tlF : $@convention(thin) <T> (Builtin.Int64, @in_guaranteed T) -> Builtin.Int64 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Int64, [[ARG1:%.*]] : @guaranteed $T):
// CHECK:   [[ARG1_COPY:%.*]] = copy_value [[ARG1]]
// CHECK:   [[TPL:%.*]] = tuple ([[ARG0]] : $Builtin.Int64, [[ARG1_COPY]] : $T)
// CHECK:   [[BORROWED_TPL:%.*]] = begin_borrow [[TPL]] : $(Builtin.Int64, T)
// CHECK:   [[CPY:%.*]] = copy_value [[BORROWED_TPL]] : $(Builtin.Int64, T)
// CHECK:   ([[INT:%.*]], [[GEN:%.*]]) = destructure_tuple [[CPY]] : $(Builtin.Int64, T)
// CHECK:   [[INT_LIFETIME:%.*]] = move_value [var_decl] [[INT]] : $Builtin.Int64
// CHECK:   destroy_value [[GEN]]
// CHECK:   end_borrow [[BORROWED_TPL]] : $(Builtin.Int64, T)
// CHECK:   destroy_value [[TPL]] : $(Builtin.Int64, T)
// CHECK:   return [[INT_LIFETIME]]
// CHECK-LABEL: } // end sil function '$ss19f040_tupleReturnIntyBi64_Bi64__xt_tlF'
func f040_tupleReturnInt<T>(_ x: (Builtin.Int64, T)) -> Builtin.Int64 {
  let y = x.0
  return y
}

// Test returning an opaque tuple of tuples.
// ---
// CHECK-LABEL: sil hidden [noinline] [ossa] @$ss16f050_multiResultyx_x_xttxlF : $@convention(thin) <T> (@in_guaranteed T) -> (@out T, @out T, @out T) {
// CHECK: bb0(%0 : @guaranteed $T):
// CHECK: %[[CP1:.*]] = copy_value %{{.*}} : $T
// CHECK: %[[CP2:.*]] = copy_value %{{.*}} : $T
// CHECK: %[[CP3:.*]] = copy_value %{{.*}} : $T
// CHECK-NOT: destroy_value %0 : $T
// CHECK: %[[TPL:.*]] = tuple (%[[CP1]] : $T, %[[CP2]] : $T, %[[CP3]] : $T)
// CHECK: return %[[TPL]] : $(T, T, T)
// CHECK-LABEL: } // end sil function '$ss16f050_multiResultyx_x_xttxlF'
@inline(never)
func f050_multiResult<T>(_ t: T) -> (T, (T, T)) {
  return (t, (t, t))
}

// Test returning an opaque tuple of tuples as a concrete tuple.
// ---
// CHECK-LABEL: sil [ossa] @$ss20f060_callMultiResult1iBi64__Bi64__Bi64_ttBi64__tF : $@convention(thin) (Builtin.Int64) -> (Builtin.Int64, Builtin.Int64, Builtin.Int64) {
// CHECK: bb0(%0 : $Builtin.Int64):
// CHECK: %[[FN:.*]] = function_ref @$ss16f050_multiResultyx_x_xttxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @out τ_0_0, @out τ_0_0)
// CHECK: %[[TPL:.*]] = apply %[[FN]]<Builtin.Int64>(%0) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @out τ_0_0, @out τ_0_0)
// CHECK: (%[[I1:.*]], %[[I2:.*]], %[[I3:.*]]) = destructure_tuple %[[TPL]] : $(Builtin.Int64, Builtin.Int64, Builtin.Int64)
// CHECK: %[[R:.*]] = tuple (%[[I1]] : $Builtin.Int64, %[[I2]] : $Builtin.Int64, %[[I3]] : $Builtin.Int64)
// CHECK: return %[[R]] : $(Builtin.Int64, Builtin.Int64, Builtin.Int64)
// CHECK-LABEL: } // end sil function '$ss20f060_callMultiResult1iBi64__Bi64__Bi64_ttBi64__tF'
public func f060_callMultiResult(i: Builtin.Int64) -> (Builtin.Int64, (Builtin.Int64, Builtin.Int64)) {
  return f050_multiResult(i)
}

// SILGen, prepareArchetypeCallee. Materialize a
// non-class-constrained self from a class-constrained archetype.
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss20f070_materializeSelf1tyx_tRlzCs4FooPRzlF : $@convention(thin) <T where T : AnyObject, T : FooP> (@guaranteed T) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK: [[WITNESS_METHOD:%.*]] = witness_method $T, #FooP.foo : <Self where Self : FooP> (Self) -> () -> () : $@convention(witness_method: FooP) <τ_0_0 where τ_0_0 : FooP> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply [[WITNESS_METHOD]]<T>([[ARG]]) : $@convention(witness_method: FooP) <τ_0_0 where τ_0_0 : FooP> (@in_guaranteed τ_0_0) -> ()
// CHECK-NOT: destroy_value [[ARG]] : $T
// CHECK: return %{{[0-9]+}} : $()
// CHECK-LABEL: } // end sil function '$ss20f070_materializeSelf1tyx_tRlzCs4FooPRzlF'
func f070_materializeSelf<T: FooP>(t: T) where T: AnyObject {
  t.foo()
}

// Test open existential with opaque values
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss8f080_bar1pBi64_s1P_p_tF : $@convention(thin) (@in_guaranteed any P) -> Builtin.Int64 {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any P):
// CHECK:   [[OPENED_ARG:%.*]] = open_existential_value [[ARG]] : $any P to $@opened
// CHECK:   [[WITNESS_FUNC:%.*]] = witness_method $@opened
// CHECK:   [[RESULT:%.*]] = apply [[WITNESS_FUNC]]<{{.*}}>([[OPENED_ARG]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> Builtin.Int64
// CHECK-NOT:   destroy_value [[ARG]] : $any P
// CHECK:   return [[RESULT]] : $Builtin.Int64
// CHECK-LABEL: } // end sil function '$ss8f080_bar1pBi64_s1P_p_tF'
func f080_bar(p: P) -> Builtin.Int64 {
  return p.x
}

// Test OpaqueTypeLowering copyValue and destroyValue.
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss11f090_calleryxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK-NOT: copy_value
// CHECK:   [[RESULT:%.*]] = apply {{%.*}}<T>([[ARG]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK-NOT:   destroy_value [[ARG]] : $T
// CHECK:   return %{{.*}} : $T
// CHECK-LABEL: } // end sil function '$ss11f090_calleryxxlF'
func f090_caller<T>(_ t: T) -> T {
  return f090_caller(t)
}

// Test a simple opaque parameter and return value.
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss13f100_identityyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]] : $T
// CHECK-NOT:   destroy_value [[ARG]] : $T
// CHECK:   return [[COPY_ARG]] : $T
// CHECK-LABEL: } // end sil function '$ss13f100_identityyxxlF'
func f100_identity<T>(_ t: T) -> T {
  return t
}

// Test a guaranteed opaque parameter.
// ---
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$ss19f110_GuaranteedSelfVs4FooPssACP3fooyyFTW : $@convention(witness_method: FooP) (@in_guaranteed f110_GuaranteedSelf) -> () {
// CHECK: bb0(%0 : $f110_GuaranteedSelf):
// CHECK:   %[[F:.*]] = function_ref @$ss19f110_GuaranteedSelfV3fooyyF : $@convention(method) (f110_GuaranteedSelf) -> ()
// CHECK:   apply %[[F]](%0) : $@convention(method) (f110_GuaranteedSelf) -> ()
// CHECK:   return
// CHECK-LABEL: } // end sil function '$ss19f110_GuaranteedSelfVs4FooPssACP3fooyyFTW'
struct f110_GuaranteedSelf : FooP {
  func foo() {}
}

// Tests a corner case wherein we used to do a temporary and return a pointer to T instead of T
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss16f120_returnValueyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   [[COPY_ARG1:%.*]] = copy_value [[ARG]] : $T
// CHECK:   [[MOVE:%.*]] = move_value [lexical] [var_decl] [[COPY_ARG1]] : $T
// CHECK:   [[BORROWED_ARG2:%.*]] = begin_borrow [[MOVE]]
// CHECK:   [[COPY_ARG2:%.*]] = copy_value [[BORROWED_ARG2]] : $T
// CHECK:   end_borrow [[BORROWED_ARG2]]
// CHECK:   return [[COPY_ARG2]] : $T
// CHECK-LABEL: } // end sil function '$ss16f120_returnValueyxxlF'
func f120_returnValue<T>(_ x: T) -> T {
  let y = x
  return y
}

// Tests Optional initialization by value
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss9f130_wrapyxSgxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out Optional<T> {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]] : $T
// CHECK:   [[OPTIONAL_ARG:%.*]] = enum $Optional<T>, #Optional.some!enumelt, [[COPY_ARG]] : $T
// CHECK-NOT:   destroy_value [[ARG]] : $T
// CHECK:   return [[OPTIONAL_ARG]] : $Optional<T>
// CHECK-LABEL: } // end sil function '$ss9f130_wrapyxSgxlF'
func f130_wrap<T>(_ x: T) -> T? {
  return x
}

func f150_anyArg(_: Any) {}

// Tests init of opaque existentials
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss15f160_callAnyArgyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[INT_LIT:%.*]] = integer_literal $Builtin.IntLiteral, 42
// CHECK:   [[INT_TYPE:%.*]] = metatype $@thin Int64.Type
// CHECK:   [[INT_ARG:%.*]] = apply %{{.*}}([[INT_LIT]], [[INT_TYPE]]) : $@convention(method) (Builtin.IntLiteral, @thin Int64.Type) -> Int64
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[INT_ARG]] : $Int64, $Int64, $Any
// CHECK:   apply %{{.*}}([[INIT_OPAQUE]]) : $@convention(thin) (@in_guaranteed Any) -> ()
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss15f160_callAnyArgyyF'
func f160_callAnyArg() {
  f150_anyArg(Int64(42))
}

// Tests unconditional_checked_cast for opaque values
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss18f170_force_convertxylF : $@convention(thin) <T> () -> @out T {
// CHECK: bb0:
// CHECK-NOT: alloc_stack
// CHECK:   [[INT_LIT:%.*]] = integer_literal $Builtin.IntLiteral, 42
// CHECK:   [[INT_TYPE:%.*]] = metatype $@thin Int64.Type
// CHECK:   [[INT_ARG:%.*]] = apply %{{.*}}([[INT_LIT]], [[INT_TYPE]]) : $@convention(method) (Builtin.IntLiteral, @thin Int64.Type) -> Int64
// CHECK:   [[INT_CAST:%.*]] = unconditional_checked_cast [[INT_ARG]] : $Int64 to T
// CHECK:   [[CAST_LIFETIME:%.*]] = move_value [lexical] [var_decl] [[INT_CAST]] : $T
// CHECK:   [[CAST_BORROW:%.*]] = begin_borrow [[CAST_LIFETIME]] : $T
// CHECK:   [[RETURN_VAL:%.*]] = copy_value [[CAST_BORROW]] : $T
// CHECK:   end_borrow [[CAST_BORROW]] : $T
// CHECK:   destroy_value [[CAST_LIFETIME]] : $T
// CHECK:   return [[RETURN_VAL]] : $T
// CHECK-LABEL: } // end sil function '$ss18f170_force_convertxylF'
func f170_force_convert<T>() -> T {
  let x : T = Int64(42) as! T
  return x
}

// Tests supporting function for f190_return_foo_var - cast and return of protocol
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss15f180_return_foos4FooP_pyF : $@convention(thin) () -> @out any FooP {
// CHECK: bb0:
// CHECK:   [[INT_LIT:%.*]] = integer_literal $Builtin.IntLiteral, 42
// CHECK:   [[INT_TYPE:%.*]] = metatype $@thin Int64.Type
// CHECK:   [[INT_ARG:%.*]] = apply %{{.*}}([[INT_LIT]], [[INT_TYPE]]) : $@convention(method) (Builtin.IntLiteral, @thin Int64.Type) -> Int64
// CHECK:   [[INT_CAST:%.*]] = unconditional_checked_cast [[INT_ARG]] : $Int64 to any FooP
// CHECK:   return [[INT_CAST]] : $any FooP
// CHECK-LABEL: } // end sil function '$ss15f180_return_foos4FooP_pyF'
func f180_return_foo() -> FooP {
  return Int64(42) as! FooP
}
var foo_var : FooP = f180_return_foo()

// Tests return of global variables by doing a load of copy
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss19f190_return_foo_vars4FooP_pyF : $@convention(thin) () -> @out any FooP {
// CHECK: bb0:
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @$ss7foo_vars4FooP_pvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[RAW:%.*]] = apply [[ADDRESSOR]]()
// CHECK:   [[GLOBAL:%.*]] = pointer_to_address [[RAW]] : $Builtin.RawPointer to [strict] $*any FooP
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL]] : $*any FooP
// CHECK:   [[LOAD_GLOBAL:%.*]] = load [copy] [[READ]] : $*any FooP
// CHECK:   return [[LOAD_GLOBAL]] : $any FooP
// CHECK-LABEL: } // end sil function '$ss19f190_return_foo_vars4FooP_pyF'
func f190_return_foo_var() -> FooP {
  return foo_var
}

// Tests deinit of opaque existentials
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss16f200_use_foo_varyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @$ss7foo_vars4FooP_pvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[RAW:%.*]] = apply [[ADDRESSOR]]()
// CHECK:   [[GLOBAL:%.*]] = pointer_to_address [[RAW]] : $Builtin.RawPointer to [strict] $*any FooP
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL]] : $*any FooP
// CHECK:   [[LOAD_GLOBAL:%.*]] = load [copy] [[READ]] : $*any FooP
// CHECK:   [[BORROW:%.*]] = begin_borrow [[LOAD_GLOBAL]] : $any FooP
// CHECK:   [[OPEN_VAR:%.*]] = open_existential_value [[BORROW]] : $any FooP
// CHECK:   [[WITNESS:%.*]] = witness_method $@opened
// CHECK:   apply [[WITNESS]]
// CHECK:   end_borrow [[BORROW]]
// CHECK:   destroy_value [[LOAD_GLOBAL]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss16f200_use_foo_varyyF'
func f200_use_foo_var() {
  foo_var.foo()
}


// Tests composition erasure of opaque existentials + copy into of opaques
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss16f210_compErasureys5Error_psAB_s4FooPpF : $@convention(thin) (@in_guaranteed any Error & FooP) -> @owned any Error {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any Error & FooP):
// CHECK:   [[OPAQUE_ARG:%.*]] = open_existential_value [[ARG]] : $any Error & FooP to $@opened({{.*}}, any Error & FooP) Self
// CHECK:   [[TEMP:%.*]] = alloc_stack $any Error
// CHECK:   [[EXIST_BOX:%.*]] = alloc_existential_box $any Error, $@opened({{.*}}, any Error & FooP) Self
// CHECK:   [[PROJ_BOX:%.*]] = project_existential_box $@opened({{.*}}, any Error & FooP) Self in [[EXIST_BOX]]
// CHECK:   store [[EXIST_BOX]] to [init] [[TEMP]] : $*any Error
// CHECK:   [[COPY_OPAQUE:%.*]] = copy_value [[OPAQUE_ARG]] : $@opened({{.*}}, any Error & FooP) Self
// CHECK:   store [[COPY_OPAQUE]] to [init] [[PROJ_BOX]] : $*@opened({{.*}}, any Error & FooP) Self
// CHECK:   [[RESULT:%.*]] = load [take] [[TEMP]] : $*any Error
// CHECK-NOT:   destroy_value [[ARG]] : $any Error & FooP
// CHECK:   return [[RESULT]] : $any Error
// CHECK-LABEL: } // end sil function '$ss16f210_compErasureys5Error_psAB_s4FooPpF'
func f210_compErasure(_ x: FooP & Error) -> Error {
  return x
}

// Tests Implicit Value Construction under Opaque value mode
// ---

// f250_testBoxT continued Test Implicit Value Construction under Opaque value mode
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss3BoxV1tAByxGx_tcfC : $@convention(method) <T> (@in T, @thin Box<T>.Type) -> @out Box<T> {
// CHECK: bb0([[ARG0:%.*]] : @owned $T, [[ARG1:%.*]] : $@thin Box<T>.Type):
// CHECK:   [[RETVAL:%.*]] = struct $Box<T> ([[ARG0]] : $T)
// CHECK:   return [[RETVAL]] : $Box<T>
// CHECK-LABEL: } // end sil function '$ss3BoxV1tAByxGx_tcfC'
struct Box<T> {
  let t: T
}

// CHECK-LABEL: sil hidden [ossa] @$ss13f250_testBoxTyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[BOX_MTYPE:%.*]] = metatype $@thin Box<Int64>.Type
// CHECK:   [[INTLIT:%.*]] = integer_literal $Builtin.IntLiteral, 42
// CHECK:   [[MTYPE:%.*]] = metatype $@thin Int64.Type
// CHECK:   [[AINT:%.*]] = apply {{.*}}([[INTLIT]], [[MTYPE]]) : $@convention(method) (Builtin.IntLiteral, @thin Int64.Type) -> Int64
// CHECK:   apply {{.*}}<Int64>([[AINT]], [[BOX_MTYPE]]) : $@convention(method) <τ_0_0> (@in τ_0_0, @thin Box<τ_0_0>.Type) -> @out Box<τ_0_0>
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss13f250_testBoxTyyF'
func f250_testBoxT() {
  let _ = Box(t: Int64(42))
}

enum AddressOnlyEnum {
  case nought
  case mere(EmptyP)
  case phantom(AddressOnlyStruct)
}

// Tests Address only enums
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss15f260_AOnly_enumyys17AddressOnlyStructVF : $@convention(thin) (AddressOnlyStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : $AddressOnlyStruct):
// CHECK:   [[MTYPE1:%.*]] = metatype $@thin AddressOnlyEnum.Type
// CHECK:   [[APPLY1:%.*]] = apply {{.*}}([[MTYPE1]]) : $@convention(thin) (@thin AddressOnlyEnum.Type) -> @owned @callee_guaranteed (@in_guaranteed any EmptyP) -> @out AddressOnlyEnum
// CHECK:   destroy_value [[APPLY1]]
// CHECK:   [[MTYPE2:%.*]] = metatype $@thin AddressOnlyEnum.Type
// CHECK:   [[ENUM1:%.*]] = enum $AddressOnlyEnum, #AddressOnlyEnum.nought!enumelt
// CHECK:   [[MTYPE3:%.*]] = metatype $@thin AddressOnlyEnum.Type
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[ARG]] : $AddressOnlyStruct, $AddressOnlyStruct, $any EmptyP
// CHECK:   [[ENUM2:%.*]] = enum $AddressOnlyEnum, #AddressOnlyEnum.mere!enumelt, [[INIT_OPAQUE]] : $any EmptyP
// CHECK:   destroy_value [[ENUM2]]
// CHECK:   [[MTYPE4:%.*]] = metatype $@thin AddressOnlyEnum.Type
// CHECK:   [[ENUM3:%.*]] = enum $AddressOnlyEnum, #AddressOnlyEnum.phantom!enumelt, [[ARG]] : $AddressOnlyStruct
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss15f260_AOnly_enumyys17AddressOnlyStructVF'
func f260_AOnly_enum(_ s: AddressOnlyStruct) {
  _ = AddressOnlyEnum.mere

  _ = AddressOnlyEnum.nought

  _ = AddressOnlyEnum.mere(s)

  _ = AddressOnlyEnum.phantom(s)
}

// Tests InjectOptional for opaque value types + conversion of opaque structs
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss21f270_convOptAnyStructyys0dE0VACSgcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (@in_guaranteed Optional<AnyStruct>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct) -> @out Optional<AnyStruct>
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out Optional<AnyStruct>
// CHECK-NOT:   destroy_value [[ARG]] : $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss21f270_convOptAnyStructyys0dE0VACSgcF'
func f270_convOptAnyStruct(_ a1: @escaping (AnyStruct?) -> AnyStruct) {
  let _: (AnyStruct?) -> AnyStruct? = a1
}

// f270_convOptAnyStruct continued Test: reabstraction thunk helper
// ---
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$ss9AnyStructVSgABIegnr_A2CIegnr_TR : $@convention(thin) (@in_guaranteed Optional<AnyStruct>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct) -> @out Optional<AnyStruct> {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Optional<AnyStruct>, [[ARG1:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct):
// CHECK:   [[APPLYARG:%.*]] = apply [[ARG1]]([[ARG0]]) : $@callee_guaranteed (@in_guaranteed Optional<AnyStruct>) -> @out AnyStruct
// CHECK:   [[RETVAL:%.*]] = enum $Optional<AnyStruct>, #Optional.some!enumelt, [[APPLYARG]] : $AnyStruct
// CHECK:   return [[RETVAL]] : $Optional<AnyStruct>
// CHECK-LABEL: } // end sil function '$ss9AnyStructVSgABIegnr_A2CIegnr_TR'

// Tests conversion between existential types
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss21f280_convExistTrivialyys0D6StructVs1P_pcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed any P) -> TrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed any P) -> TrivialStruct):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (@in_guaranteed any P2, @guaranteed @callee_guaranteed (@in_guaranteed any P) -> TrivialStruct) -> @out any P2
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed (@in_guaranteed any P2) -> @out any P2
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss21f280_convExistTrivialyys0D6StructVs1P_pcF'
func f280_convExistTrivial(_ s: @escaping (P) -> TrivialStruct) {
  let _: (P2) -> P2 = s
}

// part of f280_convExistTrivial: conversion between existential types - reabstraction thunk
// ---
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$ss1P_ps13TrivialStructVIegnd_s2P2_psAD_pIegnr_TR : $@convention(thin) (@in_guaranteed any P2, @guaranteed @callee_guaranteed (@in_guaranteed any P) -> TrivialStruct) -> @out any P2 {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $any P2, [[ARG1:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed any P) -> TrivialStruct):
// CHECK:   [[OPENED_ARG:%.*]] = open_existential_value [[ARG]] : $any P2 to $@opened({{.*}}, any P2) Self
// CHECK:   [[COPIED_VAL:%.*]] = copy_value [[OPENED_ARG]]
// CHECK:   [[INIT_P:%.*]] = init_existential_value [[COPIED_VAL]] : $@opened({{.*}}, any P2) Self, $@opened({{.*}}, any P2) Self, $any P
// CHECK:   [[BORROWED_INIT_P:%.*]] = begin_borrow [[INIT_P]]
// CHECK:   [[APPLY_P:%.*]] = apply [[ARG1]]([[BORROWED_INIT_P]]) : $@callee_guaranteed (@in_guaranteed any P) -> TrivialStruct
// CHECK:   [[RETVAL:%.*]] = init_existential_value [[APPLY_P]] : $TrivialStruct, $TrivialStruct, $any P2
// CHECK:   end_borrow [[BORROWED_INIT_P]]
// CHECK-NOT:   destroy_value [[ARG0]]
// CHECK:   return [[RETVAL]] : $any P2
// CHECK-LABEL: } // end sil function '$ss1P_ps13TrivialStructVIegnd_s2P2_psAD_pIegnr_TR'

// Tests conversion between existential types - optionals case
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss21f290_convOptExistTrivyys13TrivialStructVs1P_pSgcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed Optional<any P>) -> TrivialStruct) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed Optional<any P>) -> TrivialStruct):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (Optional<TrivialStruct>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<any P>) -> TrivialStruct) -> @out any P2
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed (Optional<TrivialStruct>) -> @out any P2
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss21f290_convOptExistTrivyys13TrivialStructVs1P_pSgcF'
func f290_convOptExistTriv(_ s: @escaping (P?) -> TrivialStruct) {
  let _: (TrivialStruct?) -> P2 = s
}

// part of f290_convOptExistTriv: conversion between existential types - reabstraction thunk - optionals case
// ---
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$ss1P_pSgs13TrivialStructVIegnd_ADSgs2P2_pIegyr_TR : $@convention(thin) (Optional<TrivialStruct>, @guaranteed @callee_guaranteed (@in_guaranteed Optional<any P>) -> TrivialStruct) -> @out any P2 {
// CHECK: bb0([[ARG0:%.*]] : $Optional<TrivialStruct>, [[ARG1:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed Optional<any P>) -> TrivialStruct):
// CHECK:   switch_enum [[ARG0]] : $Optional<TrivialStruct>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
// CHECK: [[SOME_BB]]([[OSOME:%.*]] : $TrivialStruct):
// CHECK:   [[INIT_S:%.*]] = init_existential_value [[OSOME]] : $TrivialStruct, $TrivialStruct, $any P
// CHECK:   [[ENUM_S:%.*]] = enum $Optional<any P>, #Optional.some!enumelt, [[INIT_S]] : $any P
// CHECK:   br [[CONT_BB:bb[0-9]+]]([[ENUM_S]] : $Optional<any P>)
// CHECK: [[NONE_BB]]:
// CHECK:   [[ONONE:%.*]] = enum $Optional<any P>, #Optional.none!enumelt
// CHECK:   br [[CONT_BB]]([[ONONE]] : $Optional<any P>)
// CHECK: [[CONT_BB]]([[OPT_S:%.*]] : @owned $Optional<any P>):
// CHECK:   [[BORROWED_OPT_S:%.*]] = begin_borrow [[OPT_S]]
// CHECK:   [[APPLY_P:%.*]] = apply [[ARG1]]([[BORROWED_OPT_S]]) : $@callee_guaranteed (@in_guaranteed Optional<any P>) -> TrivialStruct
// CHECK:   [[RETVAL:%.*]] = init_existential_value [[APPLY_P]] : $TrivialStruct, $TrivialStruct, $any P2
// CHECK:   return [[RETVAL]] : $any P2
// CHECK-LABEL: } // end sil function '$ss1P_pSgs13TrivialStructVIegnd_ADSgs2P2_pIegyr_TR'

// Tests corner-case: reabstraction of an empty tuple to any
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss20f300_convETupleToAnyyyyycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed () -> ()):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @out Any
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed () -> @out Any
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss20f300_convETupleToAnyyyyycF'
func f300_convETupleToAny(_ t: @escaping () -> ()) {
  let _: () -> Any = t
}

// f300_convETupleToAny continued Test: reabstraction of () to Any
// ---
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIeg_ypIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @out Any {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed () -> ()):
// CHECK:   [[ASTACK:%.*]] = alloc_stack $Any
// CHECK:   [[IADDR:%.*]] = init_existential_addr [[ASTACK]] : $*Any, $()
// CHECK:   [[APPLYARG:%.*]] = apply [[ARG]]() : $@callee_guaranteed () -> ()
// CHECK:   [[LOAD_EXIST:%.*]] = load [trivial] [[IADDR]] : $*()
// CHECK:   [[RETVAL:%.*]] = init_existential_value [[LOAD_EXIST]] : $(), $(), $Any
// CHECK:   return [[RETVAL]] : $Any
// CHECK-LABEL: } // end sil function '$sIeg_ypIegr_TR'

// Tests corner-case: reabstraction of a non-empty tuple to any
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss21f310_convnIntTupleAnyyyBi64__Bi64_tycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> (Builtin.Int64, Builtin.Int64)) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed () -> (Builtin.Int64, Builtin.Int64)):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (@guaranteed @callee_guaranteed () -> (Builtin.Int64, Builtin.Int64)) -> @out Any
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed () -> @out Any
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss21f310_convnIntTupleAnyyyBi64__Bi64_tycF'
func f310_convnIntTupleAny(_ t: @escaping () -> (Builtin.Int64, Builtin.Int64)) {
  let _: () -> Any = t
}

// f310_convIntTupleAny continued Test: reabstraction of non-empty tuple to Any
// ---
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBi64_Bi64_Iegdd_ypIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> (Builtin.Int64, Builtin.Int64)) -> @out Any {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed () -> (Builtin.Int64, Builtin.Int64)):
// CHECK:   [[ASTACK:%.*]] = alloc_stack $Any
// CHECK:   [[IADDR:%.*]] = init_existential_addr [[ASTACK]] : $*Any, $(Builtin.Int64, Builtin.Int64)
// CHECK:   [[TADDR0:%.*]] = tuple_element_addr [[IADDR]] : $*(Builtin.Int64, Builtin.Int64), 0
// CHECK:   [[TADDR1:%.*]] = tuple_element_addr [[IADDR]] : $*(Builtin.Int64, Builtin.Int64), 1
// CHECK:   [[APPLYARG:%.*]] = apply [[ARG]]() : $@callee_guaranteed () -> (Builtin.Int64, Builtin.Int64)
// CHECK:   ([[TEXTRACT0:%.*]], [[TEXTRACT1:%.*]]) = destructure_tuple [[APPLYARG]] : $(Builtin.Int64, Builtin.Int64)
// CHECK:   store [[TEXTRACT0]] to [trivial] [[TADDR0]] : $*Builtin.Int64
// CHECK:   store [[TEXTRACT1]] to [trivial] [[TADDR1]] : $*Builtin.Int64
// CHECK:   [[LOAD_EXIST:%.*]] = load [trivial] [[IADDR]] : $*(Builtin.Int64, Builtin.Int64)
// CHECK:   [[RETVAL:%.*]] = init_existential_value [[LOAD_EXIST]] : $(Builtin.Int64, Builtin.Int64), $(Builtin.Int64, Builtin.Int64), $Any
// CHECK:   dealloc_stack [[ASTACK]] : $*Any
// CHECK:   return [[RETVAL]] : $Any
// CHECK-LABEL: } // end sil function '$sBi64_Bi64_Iegdd_ypIegr_TR'

// Tests translating and imploding into Any under opaque value mode
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss20f320_transImplodeAnyyyyypcF : $@convention(thin) (@guaranteed @callee_guaranteed (@in_guaranteed Any) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed Any) -> ()):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}([[COPY_ARG]]) : $@convention(thin) (Builtin.Int64, Builtin.Int64, @guaranteed @callee_guaranteed (@in_guaranteed Any) -> ()) -> ()
// CHECK:   destroy_value [[PAPPLY]] : $@callee_guaranteed (Builtin.Int64, Builtin.Int64) -> ()
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss20f320_transImplodeAnyyyyypcF'
func f320_transImplodeAny(_ t: @escaping (Any) -> ()) {
  let _: ((Builtin.Int64, Builtin.Int64)) -> () = t
}

// f320_transImplodeAny continued Test: reabstraction thunk
// ---
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sypIegn_Bi64_Bi64_Iegyy_TR : $@convention(thin) (Builtin.Int64, Builtin.Int64, @guaranteed @callee_guaranteed (@in_guaranteed Any) -> ()) -> () {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Int64, [[ARG1:%.*]] : $Builtin.Int64, [[ARG2:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed Any) -> ()):
// CHECK:   [[ASTACK:%.*]] = alloc_stack $Any
// CHECK:   [[IADDR:%.*]] = init_existential_addr [[ASTACK]] : $*Any, $(Builtin.Int64, Builtin.Int64)
// CHECK:   [[TADDR0:%.*]] = tuple_element_addr [[IADDR]] : $*(Builtin.Int64, Builtin.Int64), 0
// CHECK:   store [[ARG0]] to [trivial] [[TADDR0]] : $*Builtin.Int64
// CHECK:   [[TADDR1:%.*]] = tuple_element_addr [[IADDR]] : $*(Builtin.Int64, Builtin.Int64), 1
// CHECK:   store [[ARG1]] to [trivial] [[TADDR1]] : $*Builtin.Int64
// CHECK:   [[LOAD_EXIST:%.*]] = load [trivial] [[IADDR]] : $*(Builtin.Int64, Builtin.Int64)
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[LOAD_EXIST]] : $(Builtin.Int64, Builtin.Int64), $(Builtin.Int64, Builtin.Int64), $Any
// CHECK:   [[BORROWED_INIT_OPAQUE:%.*]] = begin_borrow [[INIT_OPAQUE]]
// CHECK:   [[APPLYARG:%.*]] = apply [[ARG2]]([[BORROWED_INIT_OPAQUE]]) : $@callee_guaranteed (@in_guaranteed Any) -> ()
// CHECK:   dealloc_stack [[ASTACK]] : $*Any
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$sypIegn_Bi64_Bi64_Iegyy_TR'

// Tests support for address only let closures under opaque value mode - they are not by-address anymore
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss19f330_addrLetClosureyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   [[RESULT:%.*]] = apply {{%.*}}<T>([[ARG]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   return [[RESULT]] : $T
// CHECK-LABEL: } // end sil function '$ss19f330_addrLetClosureyxxlF'
func f330_addrLetClosure<T>(_ x:T) -> T {
  return { { x }() }()
}

// Tests support for capture of a mutable opaque value type
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss15f340_captureBoxyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var any EmptyP }, var, name "mutableAddressOnly"
// CHECK:   [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[ALLOC_OF_BOX]] : ${ var any EmptyP }
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[BORROW_BOX]]
// CHECK:   [[APPLY_FOR_BOX:%.*]] = apply %{{.*}}(%{{.*}}) : $@convention(method) (@thin AddressOnlyStruct.Type) -> AddressOnlyStruct
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[APPLY_FOR_BOX]] : $AddressOnlyStruct, $AddressOnlyStruct, $any EmptyP
// CHECK:   store [[INIT_OPAQUE]] to [init] [[PROJ_BOX]] : $*any EmptyP
// CHECK:   mark_function_escape [[PROJ_BOX]] : $*any EmptyP
// CHECK:   apply %{{.*}}([[BORROW_BOX]]) : $@convention(thin) (@guaranteed { var any EmptyP }) -> ()
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss15f340_captureBoxyyF'
func f340_captureBox() {
  var mutableAddressOnly: EmptyP = AddressOnlyStruct()

  func captureEverything() {
    genericInout(&mutableAddressOnly)
  }

  captureEverything()
}

// Tests support for guards and indirect enums for opaque values
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss14f360_guardEnumyys08IndirectC0OyxGlF : $@convention(thin) <T> (@guaranteed IndirectEnum<T>) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $IndirectEnum<T>):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   switch_enum [[COPY_ARG]] : $IndirectEnum<T>, case #IndirectEnum.Node!enumelt: [[NODE_BB:bb[0-9]+]], case #IndirectEnum.Nil!enumelt: [[NIL_BB:bb[0-9]+]]
//
// CHECK: [[NODE_BB]]([[EARG:%.*]] : @owned $<τ_0_0> { var τ_0_0 } <T>):
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[EARG]]
// CHECK:   [[LOAD_BOX:%.*]] = load_borrow [[PROJ_BOX]] : $*T
// CHECK:   [[COPY_BOX:%.*]] = copy_value [[LOAD_BOX]] : $T
// CHECK:   end_borrow [[LOAD_BOX]] : $T
// CHECK:   [[MOVE_BOX:%.*]] = move_value [lexical] [var_decl] [[COPY_BOX]] : $T
// CHECK:   destroy_value [[EARG]]
// CHECK:   destroy_value [[MOVE_BOX]]
// CHECK:   br [[EPILOG_BB:bb[0-9]+]]
//
// CHECK: [[NIL_BB]]:
// CHECK:   br [[EPILOG_BB]]
//
// CHECK: [[EPILOG_BB]]:
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss14f360_guardEnumyys08IndirectC0OyxGlF'
func f360_guardEnum<T>(_ e: IndirectEnum<T>) {
  do {
    guard case .Node(let x) = e else { return }
    _ = x
  }
}

// Tests contextual init() of opaque value types
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss17f370_optToOptCastyxSgABlF : $@convention(thin) <T> (@in_guaranteed Optional<T>) -> @out Optional<T> {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Optional<T>):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return [[COPY_ARG]] : $Optional<T>
// CHECK-LABEL: } // end sil function '$ss17f370_optToOptCastyxSgABlF'
func f370_optToOptCast<T>(_ x : T!) -> T? {
  return x
}

// Tests casting optional opaques to optional opaques
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss19f380_contextualInityyBi64_SgF : $@convention(thin) (Optional<Builtin.Int64>) -> () {
// CHECK: bb0([[ARG:%.*]] : $Optional<Builtin.Int64>):
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var Optional<Builtin.Int64> }, var
// CHECK:   [[BORROW_BOX:%.*]] = begin_borrow [var_decl] [[ALLOC_OF_BOX]]
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[BORROW_BOX]]
// CHECK:   store [[ARG]] to [trivial] [[PROJ_BOX]] : $*Optional<Builtin.Int64>
// CHECK:   destroy_value [[ALLOC_OF_BOX]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss19f380_contextualInityyBi64_SgF'
func f380_contextualInit(_ a : Builtin.Int64?) {
  var x: Builtin.Int64? = a
  genericInout(&x)
  _ = x
}

// Tests opaque call result types
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss19f390_addrCallResultyyxycSglF : $@convention(thin) <T> (@guaranteed Optional<@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>>) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Optional<@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>>):
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <T>
// CHECK:   [[BORROW_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[ALLOC_OF_BOX]]
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[BORROW_BOX]]
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   switch_enum [[COPY_ARG]] : $Optional<@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
// CHECK: [[SOME_BB]]([[CLOSURE:%.*]] : @owned $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>):
// CHECK:   [[BORROW_CLOSURE:%.*]] = begin_borrow [[CLOSURE]]
// CHECK:   [[CALL:%.*]] = apply [[BORROW_CLOSURE]]() : $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>
// CHECK:   [[SOME:%.*]] = enum $Optional<T>, #Optional.some!enumelt, [[CALL]] : $T
// CHECK:   br [[CONT_BB:bb[0-9]+]]([[SOME]] : $Optional<T>)
// CHECK: [[CONT_BB]]([[OPT:%.*]] : @owned $Optional<T>):
// CHECK:   store [[OPT]] to [init] [[PROJ_BOX]] : $*Optional<T>
// CHECK: [[NONE_BB]]:
// CHECK:   [[ONONE:%.*]] = enum $Optional<T>, #Optional.none!enumelt
// CHECK:   br [[CONT_BB]]([[ONONE]] : $Optional<T>)
// CHECK-LABEL: } // end sil function '$ss19f390_addrCallResultyyxycSglF'
func f390_addrCallResult<T>(_ f: (() -> T)?) {
  var x = f?()
  genericInout(&x)
  _ = x
}

// Tests reabstraction / partial apply of protocols under opaque value mode
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss16f400_maybeCloneP1cys8Clonable_p_tF : $@convention(thin) (@in_guaranteed any Clonable) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any Clonable):
// CHECK:   [[CLOSURE_FN:%.*]] = function_ref @{{.*}} : $@convention(thin) (@in_guaranteed any Clonable) -> @owned @callee_guaranteed () -> @out Optional<any Clonable>
// CHECK:   [[CLOSURE:%.*]] = apply [[CLOSURE_FN]]([[ARG]]) : $@convention(thin) (@in_guaranteed any Clonable) -> @owned @callee_guaranteed () -> @out Optional<any Clonable>
// CHECK:   destroy_value [[CLOSURE]]
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss16f400_maybeCloneP1cys8Clonable_p_tF'
func f400_maybeCloneP(c: Clonable) {
  let _: () -> Clonable? = c.maybeClone
}

// Tests global opaque values / subscript rvalues
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss20f410_globalRvalueGetyBi64_Bi64_F : $@convention(thin) (Builtin.Int64) -> Builtin.Int64 {
// CHECK: bb0([[ARG:%.*]] : $Builtin.Int64):
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @{{.*}} : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[RAW:%.*]] = apply [[ADDRESSOR]]()
// CHECK:   [[GLOBAL_ADDR:%.*]] = pointer_to_address [[RAW]] : $Builtin.RawPointer to [strict] $*Optional<any SubscriptableGet>
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]] : $*Optional<any SubscriptableGet>
// CHECK:   switch_enum_addr [[READ]] : $*Optional<any SubscriptableGet>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: {{bb[0-9]+}}
// CHECK: [[SOME_BB]]:
// CHECK:   [[PAYLOAD:%.*]] = unchecked_inplace_enum_data_addr [[READ]] : $*Optional<any SubscriptableGet>, #Optional.some!enumelt
// CHECK:   [[OPEN_ARG:%.*]] = open_existential_addr immutable_access [[PAYLOAD]] : $*any SubscriptableGet to $*@opened
// CHECK:   [[GET_OPAQUE:%.*]] = load [copy] [[OPEN_ARG]] : $*@opened
// CHECK:   [[RETVAL:%.*]] = apply %{{.*}}<@opened({{.*}}, any SubscriptableGet) Self>([[ARG]], {{%.*}}) : $@convention(witness_method: SubscriptableGet) <τ_0_0 where τ_0_0 : SubscriptableGet> (Builtin.Int64, @in_guaranteed τ_0_0) -> Builtin.Int64
// CHECK:   destroy_value [[GET_OPAQUE]]
// CHECK:   return [[RETVAL]] : $Builtin.Int64
// CHECK-LABEL: } // end sil function '$ss20f410_globalRvalueGetyBi64_Bi64_F'
func f410_globalRvalueGet(_ i : Builtin.Int64) -> Builtin.Int64 {
  return subscriptableGet![i]
}

// Tests global opaque values / subscript lvalues
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss20f420_globalLvalueGetyBi64_SgBi64_F : $@convention(thin) (Builtin.Int64) -> Optional<Builtin.Int64> {
// CHECK: bb0([[ARG:%.*]] : $Builtin.Int64):
// CHECK:   [[ADDRESSOR:%.*]] = function_ref @{{.*}} : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   [[RAW:%.*]] = apply [[ADDRESSOR]]()
// CHECK:   [[GLOBAL_ADDR:%.*]] = pointer_to_address [[RAW]] : $Builtin.RawPointer to [strict] $*Optional<any SubscriptableGetSet>
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]] : $*Optional<any SubscriptableGetSet>
// CHECK:   switch_enum_addr [[READ]] : $*Optional<any SubscriptableGetSet>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: {{bb[0-9]+}}
// CHECK: [[SOME_BB]]:
// CHECK:   [[PAYLOAD:%.*]] = unchecked_inplace_enum_data_addr [[READ]] : $*Optional<any SubscriptableGetSet>, #Optional.some!enumelt
// CHECK:   [[OPEN_ARG:%.*]] = open_existential_addr immutable_access [[PAYLOAD]] : $*any SubscriptableGetSet to $*@opened
// CHECK:   [[GET_OPAQUE:%.*]] = load_borrow [[OPEN_ARG]] : $*@opened
// CHECK:   [[RESULT:%.*]] = apply %{{.*}}<@opened({{.*}}, any SubscriptableGetSet) Self>([[ARG]], [[GET_OPAQUE]]) : $@convention(witness_method: SubscriptableGetSet) <τ_0_0 where τ_0_0 : SubscriptableGetSet> (Builtin.Int64, @in_guaranteed τ_0_0) -> Builtin.Int64
// CHECK:   end_borrow [[GET_OPAQUE]]
// CHECK:   [[RETVAL:%.*]] = enum $Optional<Builtin.Int64>, #Optional.some!enumelt, [[RESULT]] : $Builtin.Int64
// CHECK:   return [[RETVAL]] : $Optional<Builtin.Int64>
// CHECK-LABEL: } // end sil function '$ss20f420_globalLvalueGetyBi64_SgBi64_F'
func f420_globalLvalueGet(_ i : Builtin.Int64) -> Builtin.Int64? {
  return subscriptableGetSet![i]
}

// Tests tuple transformation
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss21f430_callUnreachableF1tyx_tlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   [[APPLY_T:%.*]] = apply %{{.*}}<((T) -> (), T)>() : $@convention(thin) <τ_0_0> () -> @out Optional<(Builtin.Int64, τ_0_0)>
// CHECK:   switch_enum [[APPLY_T]] : $Optional<(Builtin.Int64, (@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, ()>, T))>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
// CHECK: [[SOME_BB]]([[ENUMARG:%.*]] : @owned $(Builtin.Int64, (@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, ()>, T))):
// CHECK:   ([[TELEM0:%.*]], [[TELEM1:%.*]]) = destructure_tuple [[ENUMARG]]
// CHECK:   ([[TELEM10:%.*]], [[TELEM11:%.*]]) = destructure_tuple [[TELEM1]]
// CHECK:   [[CONVERTED:%.*]] = convert_function [[TELEM10]] {{.*}} to $@callee_guaranteed (@in_guaranteed T) -> @out ()
// CHECK:   [[PAPPLY:%.*]] = partial_apply [callee_guaranteed] %{{.*}}<T>([[CONVERTED]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> @out ()) -> ()
// CHECK:   [[PAPPLY_CONV:%.*]] = convert_function [[PAPPLY]] {{.*}} to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>
// CHECK:   [[NEWT0:%.*]] = tuple ([[PAPPLY_CONV]] : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>, [[TELEM11]] : $T)
// CHECK:   [[NEWT1:%.*]] = tuple ([[TELEM0]] : $Builtin.Int64, [[NEWT0]] : $(@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>, T))
// CHECK:   [[NEWENUM:%.*]] = enum $Optional<(Builtin.Int64, (@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>, T))>, #Optional.some!enumelt, [[NEWT1]]
// CHECK:   br [[CONT_BB:bb[0-9]+]]([[NEWENUM]] :
// CHECK: [[CONT_BB]]([[ENUMIN:%.*]] : @owned
// CHECK:   destroy_value [[ENUMIN]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss21f430_callUnreachableF1tyx_tlF'
func f430_callUnreachableF<T>(t: T) {
  let _: (Builtin.Int64, ((T) -> (), T))? = unreachableF()
}


// Further testing for conditional checked cast under opaque value mode - make sure we don't create a buffer for results
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss20f440_cleanupEmissionyyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   checked_cast_br T in [[COPY_ARG]] : $T to any EmptyP, [[CAST_OK:bb[0-9]+]], [[CAST_FAIL:bb[0-9]+]]
//
// CHECK: [[CAST_OK]]([[PTYPE:%.*]] : @owned $any EmptyP):
// CHECK:   [[PSOME:%.*]] = enum $Optional<any EmptyP>, #Optional.some!enumelt, [[PTYPE]] : $any EmptyP
// CHECK:   br [[MERGE_BB:bb[0-9]+]]([[PSOME]] : $Optional<any EmptyP>)
//
// CHECK: [[CAST_FAIL]]([[FAILVAL:%.*]] : @owned $T):
// CHECK:   destroy_value [[FAILVAL]]
// CHECK:   [[NONE1:%.*]] = enum $Optional<any EmptyP>, #Optional.none!enumelt
// CHECK:   br [[MERGE_BB]]([[NONE1]] : $Optional<any EmptyP>)
//
// CHECK: [[MERGE_BB]]([[ENUMRES:%.*]] : @owned $Optional<any EmptyP>):
// CHECK:   switch_enum [[ENUMRES]] : $Optional<any EmptyP>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]([[ENUMRES2:%.*]] : @owned $any EmptyP):
// CHECK:   [[X2:%.*]] = move_value [lexical] [var_decl] [[ENUMRES2]]
// CHECK:   destroy_value [[X2]]
// CHECK:   br [[EPILOG_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   br [[EPILOG_BB]]
//
// CHECK: [[EPILOG_BB]]:
// CHECK-NOT:   destroy_value [[ARG]]
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss20f440_cleanupEmissionyyxlF'
func f440_cleanupEmission<T>(_ x: T) {
  guard let x2 = x as? EmptyP else { return }
  _ = x2
}


// Test emitNativeToCBridgedNonoptionalValue.
// ---
// A stdlib intrinsic SILGen references when bridging an arbitrary value to
// AnyObject; declaring it makes the bridging call below get emitted rather than
// stubbed out.
@_silgen_name("_bridgeAnythingToObjectiveC")
func _bridgeAnythingToObjectiveC<T>(_ x: T) -> AnyObject
// CHECK-objc-LABEL: sil hidden [ossa] @$ss14f470_nativeToC7fromAnyyXlyp_tF : $@convention(thin) (@in_guaranteed Any) -> @owned AnyObject {
// CHECK-objc: bb0([[ARG:%.*]] : @guaranteed $Any):
// CHECK-objc: [[SRC:%.*]] = copy_value [[ARG]] : $Any
// CHECK-objc: [[BORROW:%.*]] = begin_borrow [[SRC]] : $Any
// CHECK-objc: [[OPEN:%.*]] = open_existential_value [[BORROW]] : $Any to $@opened("{{.*}}", Any) Self
// CHECK-objc: [[COPY:%.*]] = copy_value [[OPEN]] : $@opened("{{.*}}", Any) Self
// CHECK-objc: end_borrow [[BORROW]] : $Any
// CHECK-objc: [[F:%.*]] = function_ref @_bridgeAnythingToObjectiveC : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned AnyObject
// CHECK-objc: [[RET:%.*]] = apply [[F]]<@opened("{{.*}}", Any) Self>([[COPY]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned AnyObject
// CHECK-objc: destroy_value [[COPY]] : $@opened("{{.*}}", Any) Self
// CHECK-objc: destroy_value [[SRC]] : $Any
// CHECK-objc: return [[RET]] : $AnyObject
// CHECK-objc-LABEL: } // end sil function '$ss14f470_nativeToC7fromAnyyXlyp_tF'
#if _runtime(_ObjC)
func f470_nativeToC(fromAny any: Any) -> AnyObject {
  return any as AnyObject
}
#endif


// Test emitOpenExistential.
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss13f480_getError04someC0yps0C0_p_tF : $@convention(thin) (@guaranteed any Error) -> @out Any {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any Error):
// CHECK: [[VAL:%.*]] = open_existential_box_value [[ARG]] : $any Error to $@opened("{{.*}}", any Error) Self
// CHECK: [[COPY:%.*]] = copy_value [[VAL]] : $@opened("{{.*}}", any Error) Self
// CHECK: [[ANY:%.*]] = init_existential_value [[COPY]] : $@opened("{{.*}}", any Error) Self, $@opened("{{.*}}", any Error) Self, $Any
// CHECK-NOT: destroy_value [[ARG]] : $any Error
// CHECK: return [[ANY]] : $Any
// CHECK-LABEL: } // end sil function '$ss13f480_getError04someC0yps0C0_p_tF'
func f480_getError(someError: Error) -> Any {
  return someError
}

// Test visitBindOptionalExpr
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss15f500_getAnyHashys1P_pSgs14ConvertibleToP_pSgF : $@convention(thin) (@in_guaranteed Optional<any ConvertibleToP>) -> @out Optional<any P> {
// CHECK: bb0(%0 : @guaranteed $Optional<any ConvertibleToP>):
// CHECK: [[COPY:%.*]] = copy_value %0 : $Optional<any ConvertibleToP>
// CHECK: switch_enum [[COPY]] : $Optional<any ConvertibleToP>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: {{bb[0-9]+}}
// CHECK: [[SOME_BB]]([[DATA:%.*]] : @owned $any ConvertibleToP):
// CHECK: [[BORROW_DATA:%.*]] = begin_borrow [[DATA]] : $any ConvertibleToP
// CHECK: [[VAL:%.*]] = open_existential_value [[BORROW_DATA]] : $any ConvertibleToP to $@opened("{{.*}}", any ConvertibleToP) Self
// CHECK: [[WT:%.*]] = witness_method $@opened("{{.*}}", any ConvertibleToP) Self, #ConvertibleToP.asP : <Self where Self : ConvertibleToP> (Self) -> () -> any P, [[VAL]] : $@opened("{{.*}}", any ConvertibleToP) Self : $@convention(witness_method: ConvertibleToP) <τ_0_0 where τ_0_0 : ConvertibleToP> (@in_guaranteed τ_0_0) -> @out any P
// CHECK: [[AS_P:%.*]] = apply [[WT]]<@opened("{{.*}}", any ConvertibleToP) Self>([[VAL]]) : $@convention(witness_method: ConvertibleToP) <τ_0_0 where τ_0_0 : ConvertibleToP> (@in_guaranteed τ_0_0) -> @out any P
// CHECK: [[ENUM:%.*]] = enum $Optional<any P>, #Optional.some!enumelt, [[AS_P]] : $any P
// CHECK: destroy_value [[DATA]] : $any ConvertibleToP
// CHECK: br {{bb[0-9]+}}([[ENUM]] : $Optional<any P>)
// CHECK: } // end sil function '$ss15f500_getAnyHashys1P_pSgs14ConvertibleToP_pSgF'
func f500_getAnyHash(_ value: ConvertibleToP?) -> P? {
  return value?.asP()
}
public protocol FooPP {
  func foo() -> Self
}

// Test emitting a protocol witness for a method (with @in_guaranteed self) on a dependent generic type.
// ---
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$ss15f510_OpaqueSelfVyxGs5FooPPssADP3fooxyFTW : $@convention(witness_method: FooPP) <τ_0_0> (@in_guaranteed f510_OpaqueSelf<τ_0_0>) -> @out f510_OpaqueSelf<τ_0_0> {
// CHECK: bb0(%0 : @guaranteed $f510_OpaqueSelf<τ_0_0>):
// CHECK:   [[FN:%.*]] = function_ref @$ss15f510_OpaqueSelfV3fooAByxGyF : $@convention(method) <τ_0_0> (@in_guaranteed f510_OpaqueSelf<τ_0_0>) -> @out f510_OpaqueSelf<τ_0_0>
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<τ_0_0>(%0) : $@convention(method) <τ_0_0> (@in_guaranteed f510_OpaqueSelf<τ_0_0>) -> @out f510_OpaqueSelf<τ_0_0>
// CHECK:   return [[RESULT]] : $f510_OpaqueSelf<τ_0_0>
// CHECK-LABEL: } // end sil function '$ss15f510_OpaqueSelfVyxGs5FooPPssADP3fooxyFTW'
struct f510_OpaqueSelf<Base> : FooPP {
  var x: Base

  func foo() -> f510_OpaqueSelf<Base> {
    return self
  }
}

// Tests conditional value casts and correspondingly generated reabstraction thunk, with <T> types
// ---
// CHECK-LABEL: sil hidden [ossa] @$ss17f520_condTFromAnyyyyp_xtlF : $@convention(thin) <T> (@in_guaranteed Any, @in_guaranteed T) -> () {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Any, [[ARG1:%.*]] : @guaranteed $T):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG0]]
// CHECK:   checked_cast_br Any in [[COPY_ARG]] : $Any to (Int64, T) -> (Int64, T), [[SUCCESS:bb[0-9]+]], {{bb[0-9]+}}
// CHECK: [[SUCCESS]]([[CAST:%.*]] : @owned $@callee_guaranteed @substituted <{{.*}}> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @out τ_0_2 for <{{.*}}>):
// CHECK:   [[CONVERTED:%.*]] = convert_function [[CAST]] {{.*}} to $@callee_guaranteed (@in_guaranteed Int64, @in_guaranteed T) -> @out (Int64, T)
// CHECK:   [[THUNK_REF:%.*]] = function_ref @{{.*}} : $@convention(thin) <τ_0_0> (Int64, @in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed Int64, @in_guaranteed τ_0_0) -> @out (Int64, τ_0_0)) -> (Int64, @out τ_0_0)
// CHECK:   partial_apply [callee_guaranteed] [[THUNK_REF]]<T>([[CONVERTED]])
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$ss17f520_condTFromAnyyyyp_xtlF'
func f520_condTFromAny<T>(_ x: Any, _ y: T) {
  if let f = x as? (Int64, T) -> (Int64, T) {
    _ = f(Int64(42), y)
  }
}

// Make sure that we insert a destroy of the box even though we used an Builtin.Int64 type.
// CHECK-LABEL: sil [ossa] @$ss16f530_assignToVaryyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[Y_BOX:%.*]] = alloc_box ${ var Int64 }, var, name "y"
// CHECK:   [[Y_BOX_BORROW:%.*]] = begin_borrow [var_decl] [[Y_BOX]] : ${ var Int64 }
// CHECK:   [[PROJECT_Y_BOX:%.*]] = project_box [[Y_BOX_BORROW]] : ${ var Int64 }, 0
// CHECK:   [[X_BOX:%.*]] = alloc_box ${ var Any }, var, name "x"
// CHECK:   [[X_BOX_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[X_BOX]] : ${ var Any }
// CHECK:   [[PROJECT_X_BOX:%.*]] = project_box [[X_BOX_BORROW]] : ${ var Any }, 0
// CHECK:   [[ACCESS1:%.*]] = begin_access [read] [unknown] [[PROJECT_Y_BOX]] : $*Int64
// CHECK:   [[Y1:%.*]] = load [trivial] [[ACCESS1]] : $*Int64
// CHECK:   [[Y_ANY_FOR_X:%.*]] = init_existential_value [[Y1]] : $Int64, $Int64, $Any
// CHECK:   store [[Y_ANY_FOR_X]] to [init] [[PROJECT_X_BOX]]
// CHECK:   [[ACCESS2:%.*]] = begin_access [read] [unknown] [[PROJECT_Y_BOX]] : $*Int64
// CHECK:   [[Y2:%.*]] = load [trivial] [[ACCESS2]] : $*Int64
// CHECK:   [[Y_ANY_FOR_Z:%.*]] = init_existential_value [[Y2]] : $Int64, $Int64, $Any
// CHECK:   [[Z:%.*]] = move_value [lexical] [var_decl] [[Y_ANY_FOR_Z]] : $Any
// CHECK:   destroy_value [[Z]]
// CHECK:   destroy_value [[X_BOX]]
// CHECK:   destroy_value [[Y_BOX]]
// CHECK: } // end sil function '$ss16f530_assignToVaryyF'
public func f530_assignToVar() {
  var y: Int64 = 3
  var x: Any = y
  let z: Any = y
  genericInout(&y)
  genericInout(&x)
  _ = z
}

// Test open_existential_value ownership
// ---
// CHECK-LABEL: sil [ossa] @$ss16f540_takeDecoder4fromBi1_s0C0_p_tKF : $@convention(thin) (@in_guaranteed any Decoder) -> (Builtin.Int1, @error any Error) {
// CHECK: bb0(%0 : @guaranteed $any Decoder):
// CHECK:  [[OPENED:%.*]] = open_existential_value %0 : $any Decoder to $@opened("{{.*}}", any Decoder) Self
// CHECK:  [[WT:%.*]] = witness_method $@opened("{{.*}}", any Decoder) Self, #Decoder.unkeyedContainer : <Self where Self : Decoder> (Self) -> () throws -> any UnkeyedDecodingContainer, [[OPENED]] : $@opened("{{.*}}", any Decoder) Self : $@convention(witness_method: Decoder) <τ_0_0 where τ_0_0 : Decoder> (@in_guaranteed τ_0_0) -> (@out any UnkeyedDecodingContainer, @error any Error)
// CHECK:  try_apply [[WT]]<@opened("{{.*}}", any Decoder) Self>([[OPENED]]) : $@convention(witness_method: Decoder) <τ_0_0 where τ_0_0 : Decoder> (@in_guaranteed τ_0_0) -> (@out any UnkeyedDecodingContainer, @error any Error), normal [[NORMAL_BB:bb[0-9]+]], error {{bb[0-9]+}}
//
// CHECK: [[NORMAL_BB]]([[RET1:%.*]] : @owned $any UnkeyedDecodingContainer):
// CHECK:  [[CONTAINER:%.*]] = move_value [lexical] [var_decl] [[RET1]] : $any UnkeyedDecodingContainer
// CHECK:  [[BORROW2:%.*]] = begin_borrow [[CONTAINER]] : $any UnkeyedDecodingContainer
// CHECK:  [[OPENED2:%.*]] = open_existential_value [[BORROW2]] : $any UnkeyedDecodingContainer to $@opened("{{.*}}", any UnkeyedDecodingContainer) Self
// CHECK:  [[WT2:%.*]] = witness_method $@opened("{{.*}}", any UnkeyedDecodingContainer) Self, #UnkeyedDecodingContainer.isAtEnd!getter : <Self where Self : UnkeyedDecodingContainer> (Self) -> () -> Builtin.Int1, [[OPENED2]] : $@opened("{{.*}}", any UnkeyedDecodingContainer) Self : $@convention(witness_method: UnkeyedDecodingContainer) <τ_0_0 where τ_0_0 : UnkeyedDecodingContainer> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK:  [[RET2:%.*]] = apply [[WT2]]<@opened("{{.*}}", any UnkeyedDecodingContainer) Self>([[OPENED2]]) : $@convention(witness_method: UnkeyedDecodingContainer) <τ_0_0 where τ_0_0 : UnkeyedDecodingContainer> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK:  end_borrow [[BORROW2]] : $any UnkeyedDecodingContainer
// CHECK:  destroy_value [[CONTAINER]] : $any UnkeyedDecodingContainer
// CHECK-NOT:  destroy_value %0 : $any Decoder
// CHECK:  return [[RET2]] : $Builtin.Int1
// CHECK-LABEL: } // end sil function '$ss16f540_takeDecoder4fromBi1_s0C0_p_tKF'
public func f540_takeDecoder(from decoder: Decoder) throws -> Builtin.Int1 {
  let container = try decoder.unkeyedContainer()
  return container.isAtEnd
}
