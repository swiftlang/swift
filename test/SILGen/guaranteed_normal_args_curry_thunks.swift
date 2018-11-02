// RUN: %target-swift-emit-silgen -parse-as-library -module-name Swift -parse-stdlib -enable-sil-ownership %s | %FileCheck %s

// This test checks specific codegen related to converting curry thunks to
// canonical representations when we are emitting guaranteed normal
// arguments. Eventually it will be merged into the normal SILGen tests.

//////////////////
// Declarations //
//////////////////

precedencegroup AssignmentPrecedence {
  assignment: true
}

enum Optional<T> {
case none
case some(T)
}

func _diagnoseUnexpectedNilOptional(_filenameStart: Builtin.RawPointer,
                                    _filenameLength: Builtin.Word,
                                    _filenameIsASCII: Builtin.Int1,
                                    _line: Builtin.Word) {
  // This would usually contain an assert, but we don't need one since we are
  // just emitting SILGen.
}

class Klass {
  init() {}
}

typealias AnyObject = Builtin.AnyObject

protocol ProtocolInitNoArg {
  init()
}

protocol ClassProtocolInitNoArg : class {
  init()
}

protocol ProtocolInitAddressOnly {
  associatedtype SubType : ProtocolInitNoArg

  init(t: SubType)
  init(t2: AddrOnlyStructInitGeneric<Klass>)
  init(t3: AddrOnlyStructInitGeneric<SubType>)
}

protocol ProtocolInitClassProtocol {
  associatedtype SubType : ClassProtocolInitNoArg

  init(t: SubType)
}

protocol ProtocolInitLoadable {
  init(t: Klass)
}

protocol ClassProtocolInitAddressOnly : class {
  associatedtype SubType : ProtocolInitNoArg

  init(t: SubType)
}

protocol ClassProtocolInitClassProtocol : class {
  associatedtype SubType : ClassProtocolInitNoArg

  init(t: SubType)
}

protocol ClassProtocolInitLoadable : class {
  init(t: Klass)
}

class LoadableClassInitLoadable {
  init(_ k: Klass) {}
}

struct LoadableStructInitLoadable {
  var k: Klass

  init(_ newK: Klass) {
    k = newK
  }
}

struct AddrOnlyStructInitGeneric<T> {
  var k: T

  init(_ newK: T) {
    k = newK
  }
}

///////////
// Tests //
///////////

// CHECK-LABEL: sil hidden @$Ss021testLoadableClassInitB0yyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[THUNK_REF:%.*]] = function_ref @$Ss017LoadableClassInitA0CyABs5KlassCcfCTcTd : $@convention(thin) (@thick LoadableClassInitLoadable.Type) -> @owned @callee_guaranteed (@guaranteed Klass) -> @owned LoadableClassInitLoadable
// CHECK:   [[CANONICAL_THUNK:%.*]] = apply [[THUNK_REF]](
// CHECK:   [[BORROWED_CANONICAL_THUNK:%.*]] = begin_borrow [[CANONICAL_THUNK]]
// CHECK:   [[BORROWED_CANONICAL_THUNK_COPY:%.*]] = copy_value [[BORROWED_CANONICAL_THUNK]]
// CHECK:   [[ALLOCATING_INIT:%.*]] = function_ref @$Ss5KlassCABycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK:   [[SELF:%.*]] = apply [[ALLOCATING_INIT]](
// CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK:   [[BORROWED_CANONICAL_THUNK:%.*]] = begin_borrow [[BORROWED_CANONICAL_THUNK_COPY]]
// CHECK:   apply [[BORROWED_CANONICAL_THUNK]]([[BORROWED_SELF]])
// CHECK: } // end sil function '$Ss021testLoadableClassInitB0yyF'

// Curry thunk.
//
// CHECK-LABEL: sil shared [thunk] @$Ss017LoadableClassInitA0CyABs5KlassCcfCTcTd : $@convention(thin) (@thick LoadableClassInitLoadable.Type) -> @owned @callee_guaranteed (@guaranteed Klass) -> @owned LoadableClassInitLoadable {
// CHECK:   [[ALLOCATING_INIT_FN:%.*]] = function_ref @$Ss017LoadableClassInitA0CyABs5KlassCcfC :
// CHECK:   [[METATYPE_PA_ALLOCATING_INIT_FN:%.*]] = partial_apply [callee_guaranteed] [[ALLOCATING_INIT_FN]](
// CHECK:   [[THUNK_FN:%.*]] = function_ref @$Ss5KlassCs017LoadableClassInitB0CIegxo_AbDIeggo_TR :
// CHECK:   [[THUNK_PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]([[METATYPE_PA_ALLOCATING_INIT_FN]]) : $@convention(thin) (@guaranteed Klass, @guaranteed @callee_guaranteed (@owned Klass) -> @owned LoadableClassInitLoadable) -> @owned LoadableClassInitLoadable
// CHECK:   return [[THUNK_PA]]
// CHECK: } // end sil function '$Ss017LoadableClassInitA0CyABs5KlassCcfCTcTd'

// Canonical Thunk.
//
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$Ss5KlassCs017LoadableClassInitB0CIegxo_AbDIeggo_TR : $@convention(thin) (@guaranteed Klass, @guaranteed @callee_guaranteed (@owned Klass) -> @owned LoadableClassInitLoadable) -> @owned LoadableClassInitLoadable {
// CHECK: bb0([[CLASS:%.*]] : @guaranteed $Klass, [[PA:%.*]] : @guaranteed $@callee_guaranteed (@owned Klass) -> @owned LoadableClassInitLoadable):
// CHECK:   [[CLASS_COPY:%.*]] = copy_value [[CLASS]]
// CHECK:   [[RESULT:%.*]] = apply [[PA]]([[CLASS_COPY]])
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$Ss5KlassCs017LoadableClassInitB0CIegxo_AbDIeggo_TR'
func testLoadableClassInitLoadable() {
  let x = LoadableClassInitLoadable.init
  let y = x(Klass())
}

// CHECK-LABEL: sil hidden @$Ss022testLoadableStructInitB0yyF : $@convention(thin) () -> () {
// CHECK:   [[THUNK_REF:%.*]] = function_ref @$Ss018LoadableStructInitA0VyABs5KlassCcfCTc : $@convention(thin) (@thin LoadableStructInitLoadable.Type) -> @owned @callee_guaranteed (@guaranteed Klass) -> @owned LoadableStructInitLoadable
// CHECK:   [[CANONICAL_THUNK:%.*]] = apply [[THUNK_REF]](
// CHECK:   [[BORROWED_CANONICAL_THUNK:%.*]] = begin_borrow [[CANONICAL_THUNK]]
// CHECK:   [[BORROWED_CANONICAL_THUNK_COPY:%.*]] = copy_value [[BORROWED_CANONICAL_THUNK]]
// CHECK:   [[ALLOCATING_INIT:%.*]] = function_ref @$Ss5KlassCABycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK:   [[SELF:%.*]] = apply [[ALLOCATING_INIT]](
// CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK:   [[BORROWED_CANONICAL_THUNK:%.*]] = begin_borrow [[BORROWED_CANONICAL_THUNK_COPY]]
// CHECK:   apply [[BORROWED_CANONICAL_THUNK]]([[BORROWED_SELF]])
// CHECK: } // end sil function '$Ss022testLoadableStructInitB0yyF'

// Curry thunk.
//
// CHECK-LABEL: sil shared [thunk] @$Ss018LoadableStructInitA0VyABs5KlassCcfCTc : $@convention(thin) (@thin LoadableStructInitLoadable.Type) -> @owned @callee_guaranteed (@guaranteed Klass) -> @owned LoadableStructInitLoadable {
// CHECK:   [[ALLOCATING_INIT_FN:%.*]] = function_ref @$Ss018LoadableStructInitA0VyABs5KlassCcfC :
// CHECK:   [[METATYPE_PA_ALLOCATING_INIT_FN:%.*]] = partial_apply [callee_guaranteed] [[ALLOCATING_INIT_FN]](
// CHECK:   [[THUNK_FN:%.*]] = function_ref @$Ss5KlassCs018LoadableStructInitB0VIegxo_AbDIeggo_TR : $@convention(thin) (@guaranteed Klass, @guaranteed @callee_guaranteed (@owned Klass) -> @owned LoadableStructInitLoadable) -> @owned LoadableStructInitLoadable
// CHECK:   [[THUNK_PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]([[METATYPE_PA_ALLOCATING_INIT_FN]])
// CHECK:   return [[THUNK_PA]]
// CHECK: } // end sil function '$Ss018LoadableStructInitA0VyABs5KlassCcfCTc'

// Canonical Thunk.
//
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$Ss5KlassCs018LoadableStructInitB0VIegxo_AbDIeggo_TR : $@convention(thin) (@guaranteed Klass, @guaranteed @callee_guaranteed (@owned Klass) -> @owned LoadableStructInitLoadable) -> @owned LoadableStructInitLoadable {
// CHECK: bb0([[CLASS:%.*]] : @guaranteed $Klass, [[PA:%.*]] : @guaranteed $@callee_guaranteed (@owned Klass) -> @owned LoadableStructInitLoadable):
// CHECK:   [[CLASS_COPY:%.*]] = copy_value [[CLASS]]
// CHECK:   [[RESULT:%.*]] = apply [[PA]]([[CLASS_COPY]])
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$Ss5KlassCs018LoadableStructInitB0VIegxo_AbDIeggo_TR'
func testLoadableStructInitLoadable() {
  let x = LoadableStructInitLoadable.init
  let y = x(Klass())
}

// In this case we have a generic type that due to type lowering introduces an
// extra thunk.
//
// CHECK-LABEL: sil hidden @$Ss37testAddrOnlyStructInitGenericConcreteyyF : $@convention(thin) () -> () {
// CHECK:   [[THUNK_REF:%.*]] = function_ref @$Ss25AddrOnlyStructInitGenericVyAByxGxcfCTc : $@convention(thin) <τ_0_0> (@thin AddrOnlyStructInitGeneric<τ_0_0>.Type) -> @owned @callee_guaranteed (@in_guaranteed τ_0_0) -> @out AddrOnlyStructInitGeneric<τ_0_0>
// CHECK:   [[CURRY_THUNK:%.*]] = apply [[THUNK_REF]]<Klass>(
// CHECK:   [[CANONICAL_THUNK_REF:%.*]] = function_ref @$Ss5KlassCs25AddrOnlyStructInitGenericVyABGIegnr_AbEIeggo_TR : $@convention(thin) (@guaranteed Klass, @guaranteed @callee_guaranteed (@in_guaranteed Klass) -> @out AddrOnlyStructInitGeneric<Klass>) -> @owned AddrOnlyStructInitGeneric<Klass>
// CHECK:   [[CANONICAL_THUNK:%.*]] = partial_apply [callee_guaranteed] [[CANONICAL_THUNK_REF]]([[CURRY_THUNK]]) :
// CHECK:   [[BORROWED_CANONICAL_THUNK:%.*]] = begin_borrow [[CANONICAL_THUNK]]
// CHECK:   [[BORROWED_CANONICAL_THUNK_COPY:%.*]] = copy_value [[BORROWED_CANONICAL_THUNK]]
// CHECK:   [[ALLOCATING_INIT:%.*]] = function_ref @$Ss5KlassCABycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK:   [[SELF:%.*]] = apply [[ALLOCATING_INIT]](
// CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK:   [[BORROWED_CANONICAL_THUNK:%.*]] = begin_borrow [[BORROWED_CANONICAL_THUNK_COPY]]
// CHECK:   apply [[BORROWED_CANONICAL_THUNK]]([[BORROWED_SELF]])

// Curry thunk
//
// CHECK-LABEL: sil shared [thunk] @$Ss25AddrOnlyStructInitGenericVyAByxGxcfCTc : $@convention(thin) <T> (@thin AddrOnlyStructInitGeneric<T>.Type) -> @owned @callee_guaranteed (@in_guaranteed T) -> @out AddrOnlyStructInitGeneric<T> {
// CHECK:   [[ALLOCATING_INIT_REF:%.*]] = function_ref @$Ss25AddrOnlyStructInitGenericVyAByxGxcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin AddrOnlyStructInitGeneric<τ_0_0>.Type) -> @out AddrOnlyStructInitGeneric<τ_0_0>
// CHECK:   [[ALLOCATING_INIT:%.*]] = partial_apply [callee_guaranteed] [[ALLOCATING_INIT_REF]]<T>(
// CHECK:   [[THUNK_REF:%.*]] = function_ref @$Sxs25AddrOnlyStructInitGenericVyxGIegir_xACIegnr_lTR : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in τ_0_0) -> @out AddrOnlyStructInitGeneric<τ_0_0>) -> @out AddrOnlyStructInitGeneric<τ_0_0>
// CHECK:   [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_REF]]<T>([[ALLOCATING_INIT]])
// CHECK:   return [[THUNK]]
// CHECK: } // end sil function '$Ss25AddrOnlyStructInitGenericVyAByxGxcfCTc'

// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$Sxs25AddrOnlyStructInitGenericVyxGIegir_xACIegnr_lTR : $@convention(thin) <T> (@in_guaranteed T, @guaranteed @callee_guaranteed (@in T) -> @out AddrOnlyStructInitGeneric<T>) -> @out AddrOnlyStructInitGeneric<T> {
// CHECK: bb0([[ARG0:%.*]] : @trivial $*AddrOnlyStructInitGeneric<T>, [[ARG1:%.*]] : @trivial $*T, [[ARG2:%.*]] : @guaranteed $@callee_guaranteed (@in T) -> @out AddrOnlyStructInitGeneric<T>):
// CHECK:   [[STACK:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[ARG1]] to [initialization] [[STACK]] : $*T
// CHECK:   apply [[ARG2]]([[ARG0]], [[STACK]]) : $@callee_guaranteed (@in T) -> @out AddrOnlyStructInitGeneric<T>
// CHECK: } // end sil function '$Sxs25AddrOnlyStructInitGenericVyxGIegir_xACIegnr_lTR'

// CHECK_LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$Ss5KlassCs25AddrOnlyStructInitGenericVyABGIegnr_AbEIeggo_TR : $@convention(thin) (@guaranteed Klass, @guaranteed @callee_guaranteed (@in_guaranteed Klass) -> @out AddrOnlyStructInitGeneric<Klass>) -> @owned AddrOnlyStructInitGeneric<Klass> {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Klass, [[ARG1:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed Klass) -> @out AddrOnlyStructInitGeneric<Klass>):
// CHECK:   [[STACK:%.*]] = alloc_stack $Klass
// CHECK:   [[ARG0_COPY:%.*]] = copy_value [[ARG0]]
// CHECK:   store [[ARG0_COPY]] to [init] [[STACK]]
// CHECK:   [[STACK2:%.*]] = alloc_stack $AddrOnlyStructInitGeneric<Klass>
// CHECK:   apply [[ARG1]]([[STACK2]], [[STACK]]) : $@callee_guaranteed (@in_guaranteed Klass) -> @out AddrOnlyStructInitGeneric<Klass>
// CHECK:   [[RESULT:%.*]] = load [take] [[STACK2]]
// CHECK:   destroy_addr [[STACK]]
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$Ss5KlassCs25AddrOnlyStructInitGenericVyABGIegnr_AbEIeggo_TR'
func testAddrOnlyStructInitGenericConcrete() {
  let x = AddrOnlyStructInitGeneric<Klass>.init
  let y = x(Klass())
}

// CHECK-LABEL: sil hidden @$Ss029testAddrOnlyStructInitGenericbC01tyx_ts08Protocole7AddressC0RzlF : $@convention(thin) <T where T : ProtocolInitAddressOnly> (@in_guaranteed T) -> () {
// CHECK:   [[CURRY_THUNK_REF:%.*]] = function_ref @$Ss25AddrOnlyStructInitGenericVyAByxGxcfCTc : $@convention(thin) <τ_0_0> (@thin AddrOnlyStructInitGeneric<τ_0_0>.Type) -> @owned @callee_guaranteed (@in_guaranteed τ_0_0) -> @out AddrOnlyStructInitGeneric<τ_0_0>
// CHECK:   [[CURRY_THUNK:%.*]] = apply [[CURRY_THUNK_REF]]<T.SubType>(
// CHECK:   [[Y:%.*]] = alloc_stack $AddrOnlyStructInitGeneric<T.SubType>, let, name "y"
// CHECK:   [[BORROWED_CURRY_THUNK:%.*]] = begin_borrow [[CURRY_THUNK]]
// CHECK:   [[COPY_BORROWED_CURRY_THUNK:%.*]] = copy_value [[BORROWED_CURRY_THUNK]]
// CHECK:   [[TMP:%.*]] = alloc_stack $T.SubType
// CHECK:   [[WITNESS_METHOD:%.*]] = witness_method $T.SubType, #ProtocolInitNoArg.init!allocator.1 : <Self where Self : ProtocolInitNoArg> (Self.Type) -> () -> Self : $@convention(witness_method: ProtocolInitNoArg) <τ_0_0 where τ_0_0 : ProtocolInitNoArg> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   apply [[WITNESS_METHOD]]<T.SubType>([[TMP]],
// CHECK:   [[BORROWED_CURRY_THUNK:%.*]] = begin_borrow [[COPY_BORROWED_CURRY_THUNK]]
// CHECK:   apply [[BORROWED_CURRY_THUNK]]([[Y]], [[TMP]])
// CHECK: } // end sil function '$Ss029testAddrOnlyStructInitGenericbC01tyx_ts08Protocole7AddressC0RzlF'
func testAddrOnlyStructInitGenericAddrOnly<T : ProtocolInitAddressOnly>(t: T) {
  let x = AddrOnlyStructInitGeneric<T.SubType>.init
  let y = x(T.SubType())
}

// CHECK-LABEL: sil hidden @$Ss20testGenericInitClass1tyx_ts08ProtocolC8LoadableRzlF : $@convention(thin) <T where T : ProtocolInitLoadable> (@in_guaranteed T) -> () {
// CHECK:   [[CURRY_THUNK_REF:%.*]] = function_ref @$Ss20ProtocolInitLoadableP1txs5KlassC_tcfCTc : $@convention(thin) <τ_0_0 where τ_0_0 : ProtocolInitLoadable> (@thick τ_0_0.Type) -> @owned @callee_guaranteed (@guaranteed Klass) -> @out τ_0_0
// CHECK:   [[CURRY_THUNK:%.*]] = apply [[CURRY_THUNK_REF]]<T>(
// CHECK:   [[Y:%.*]] = alloc_stack $T, let, name "y"
// CHECK:   [[BORROWED_CURRY_THUNK:%.*]] = begin_borrow [[CURRY_THUNK]]
// CHECK:   [[COPY_BORROWED_CURRY_THUNK:%.*]] = copy_value [[BORROWED_CURRY_THUNK]]
// CHECK:   [[ALLOCATING_INIT:%.*]] = function_ref @$Ss5KlassCABycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK:   [[SELF:%.*]] = apply [[ALLOCATING_INIT]](
// CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK:   [[BORROWED_CURRY_THUNK:%.*]] = begin_borrow [[COPY_BORROWED_CURRY_THUNK]]
// CHECK:   apply [[BORROWED_CURRY_THUNK]]([[Y]], [[BORROWED_SELF]])
// CHECK: } // end sil function '$Ss20testGenericInitClass1tyx_ts08ProtocolC8LoadableRzlF'

// Curry thunk.
//
// CHECK-LABEL: sil shared [thunk] @$Ss20ProtocolInitLoadableP1txs5KlassC_tcfCTc : $@convention(thin) <Self where Self : ProtocolInitLoadable> (@thick Self.Type) -> @owned @callee_guaranteed (@guaranteed Klass) -> @out Self {
// CHECK:   [[WITNESS_METHOD_REF:%.*]] = witness_method $Self, #ProtocolInitLoadable.init!allocator.1 : <Self where Self : ProtocolInitLoadable> (Self.Type) -> (Klass) -> Self : $@convention(witness_method: ProtocolInitLoadable) <τ_0_0 where τ_0_0 : ProtocolInitLoadable> (@owned Klass, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[WITNESS_METHOD:%.*]] = partial_apply [callee_guaranteed] [[WITNESS_METHOD_REF]]<Self>(
// CHECK:   [[CANONICAL_THUNK_REF:%.*]] = function_ref @$Ss5KlassCxIegxr_ABxIeggr_s20ProtocolInitLoadableRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : ProtocolInitLoadable> (@guaranteed Klass, @guaranteed @callee_guaranteed (@owned Klass) -> @out τ_0_0) -> @out τ_0_0
// CHECK:   [[CANONICAL_THUNK:%.*]] = partial_apply [callee_guaranteed] %3<Self>(%2) : $@convention(thin) <τ_0_0 where τ_0_0 : ProtocolInitLoadable> (@guaranteed Klass, @guaranteed @callee_guaranteed (@owned Klass) -> @out τ_0_0) -> @out τ_0_0
// CHECK:   return [[CANONICAL_THUNK]]
// CHECK: } // end sil function '$Ss20ProtocolInitLoadableP1txs5KlassC_tcfCTc'

// Canonical thunk
//
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$Ss5KlassCxIegxr_ABxIeggr_s20ProtocolInitLoadableRzlTR : $@convention(thin) <Self where Self : ProtocolInitLoadable> (@guaranteed Klass, @guaranteed @callee_guaranteed (@owned Klass) -> @out Self) -> @out Self {
// CHECK: bb0([[ARG0:%.*]] : @trivial $*Self, [[ARG1:%.*]] : @guaranteed $Klass, [[ARG2:%.*]] : @guaranteed $@callee_guaranteed (@owned Klass) -> @out Self):
// CHECK:   [[ARG1_COPY:%.*]] = copy_value [[ARG1]]
// CHECK:   apply [[ARG2]]([[ARG0]], [[ARG1_COPY]])
// CHECK: } // end sil function '$Ss5KlassCxIegxr_ABxIeggr_s20ProtocolInitLoadableRzlTR'
func testGenericInitClass<T : ProtocolInitLoadable>(t: T) {
  let x = T.init
  let y = x(Klass())
}
