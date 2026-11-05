// RUN: %target-swift-frontend %s \
// RUN: -emit-sil \
// RUN: -enable-experimental-feature OnceFunctions \
// RUN: -verify

// RUN: %if swift_feature_NoncopyableCasting %{ %target-swift-frontend %s -enable-experimental-feature NoncopyableCasting \
// RUN:     -emit-sil -verify \
// RUN:     -verify-additional-prefix casting- \
// RUN:     -target %target-future-triple \
// RUN:     -enable-experimental-feature OnceFunctions \
// RUN:     -D NONCOPYABLE_CASTING \
// RUN: %}

// RUN: %target-swift-emit-silgen -module-name test %s \
// RUN:     -enable-experimental-feature OnceFunctions 2>/dev/null \
// RUN:   | %FileCheck %s --check-prefix=SILGEN

// RUN: %if swift_feature_NoncopyableCasting %{ %target-swift-emit-silgen -enable-experimental-feature NoncopyableCasting \
// RUN:     -module-name test %s \
// RUN:     -target %target-future-triple \
// RUN:     -enable-experimental-feature OnceFunctions \
// RUN:     -D NONCOPYABLE_CASTING 2>/dev/null \
// RUN:   | %FileCheck %s --check-prefixes=SILGEN,SILGEN-CASTING \
// RUN: %}

// REQUIRES: swift_feature_OnceFunctions

struct Resource: ~Copyable {
  init() {}
  consuming func use() {}
  borrowing func peek() {}
  mutating func change() {}
}

struct Box: ~Copyable {
  private var _r: Resource
  init(_ r: consuming Resource) { _r = r }

  var r: Resource {
    consuming get { _r }
  }
}

struct Slot: ~Copyable {
  private var _r: Resource = Resource()
  
  var r: Resource {
    get { fatalError() }
    consuming set { _r = newValue }
  }
}

// `consume` operator.
// SILGEN-LABEL: sil private [ossa] @$s4test0A31ExplicitConsumeOfCapturedStruct1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testExplicitConsumeOfCapturedStruct(r: consuming Resource) {
  let g = { @callable(once) in
    let taken = consume r
    taken.use()
  }
  g()
}

// Source of an assignment.
// SILGEN-LABEL: sil private [ossa] @$s4test0A32AssignmentConsumesCapturedStruct1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testAssignmentConsumesCapturedStruct(r: consuming Resource) {
  let g = { @callable(once) in
    var local = Resource()
    local = r
    local.use()
  }
  g()
}

// Passing to a `consuming` parameter of a plain function.
// SILGEN-LABEL: sil private [ossa] @$s4test0A33ConsumingParamCallConsumesCapture1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testConsumingParamCallConsumesCapture(r: consuming Resource) {
  func consumeResource(_: consuming Resource) {}

  let g = { @callable(once) in
    consumeResource(r)
  }
  g()
}

// Passing to an initializer's implicitly-`consuming` parameter.
// SILGEN-LABEL: sil private [ossa] @$s4test0A33ConsumingInitParamConsumesCapture1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A33ConsumingInitParamConsumesCapture1ryAA8ResourceVn_tFyyXOofU0_ : $@convention(thin) (@owned Resource) -> ()
func testConsumingInitParamConsumesCapture(r: consuming Resource) {
  struct Wrapper: ~Copyable {
    let r: Resource // memberwise initializer parameter is implicitly consuming
  }

  class ConsumingWrapper {
    init(_: consuming Resource) {}
  }
  
  let g = { @callable(once) in
    let w = Wrapper(r: r)
    _ = w
  }
  g()

  let localResource = Resource()
  let h = { @callable(once) in
    let w = ConsumingWrapper(localResource)
    _ = w
  }
  h()
}

// Calling a `consuming` method (`self` is consumed).
// SILGEN-LABEL: sil private [ossa] @$s4test0A34ConsumingMethodCallConsumesCapture1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testConsumingMethodCallConsumesCapture(r: consuming Resource) {
  let g = { @callable(once) in
    r.use()
  }
  g()
}

// Intiailization consumes the value
// SILGEN-LABEL: sil private [ossa] @$s4test0A27LocalBindingConsumesCapture1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testLocalBindingConsumesCapture(r: consuming Resource) {
  let g = { @callable(once) in
    let taken = r
    taken.use()
  }
  g()
}

// SILGEN-LABEL: sil private [ossa] @$s4test27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOofU_ : $@convention(thin) (@guaranteed { var S }, @owned Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOofU0_ : $@convention(thin) (@guaranteed C, @owned Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOofU1_ : $@convention(thin) (@guaranteed { var S }, @owned Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOofU2_ : $@convention(thin) (@guaranteed C, @owned Resource) -> ()
func tesReassignmentOfProperties(r1: consuming Resource, r2: consuming Resource) {
  struct S: ~Copyable {
    var prop = Resource()
  }

  class C {
    var prop = Resource()
  }

  var s = S()
  let c = C()
  
  let g = { @callable(once) in
    s.prop = r1
  }
  g()

  let h = { @callable(once) in
    c.prop = r2
  }
  h()

  // Make sure that the same value cannot be consumed twice by different closures
  
  let r3 = Resource() // expected-error {{'r3' consumed more than once}}
  let _ = { @callable(once) in // expected-note {{consumed here}}
    s.prop = r3 
  }

  let _ = { @callable(once) in // expected-note {{consumed again here}}
    c.prop = r3
  }
}

// Capture aliasing.
// SILGEN-LABEL: sil private [ossa] @$s4test0a19CaptureListConsumesB01ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testCaptureListConsumesCapture(r: consuming Resource) {
  let g = { @callable(once) [taken = r] in
    taken.use()
  }
  g()
}

// `return` is a consuming use.
// SILGEN-LABEL: sil private [ossa] @$s4test0A21ReturnConsumesCapture1rAA8ResourceVAEn_tFAEyXOofU_ : $@convention(thin) (@owned Resource) -> @owned Resource
func testReturnConsumesCapture(r: consuming Resource) -> Resource {
  let g: @callable(once) () -> Resource = { @callable(once) in
    return r
  }
  return g()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A34BorrowingCallDoesNotConsumeCapture1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testBorrowingCallDoesNotConsumeCapture(r: consuming Resource) {
  func borrowResource(_: borrowing Resource) {}

  let g = { @callable(once) in
    r.peek() // borrowing use
    borrowResource(r)
    r.use() // consume
  }
  g()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A28DoubleConsumeOfCapturedValue1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testDoubleConsumeOfCapturedValue(r: consuming Resource) { // expected-error 2 {{'r' consumed more than once}}
  let g = { @callable(once) in
    r.use() // expected-note 2 {{consumed here}}
    r.use() // expected-note 2 {{consumed again here}}
  }
  g()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A41RegularClosureCannotConsumeCapturedStruct1ryAA8ResourceVn_tFyycfU_ : $@convention(thin) (@guaranteed { var Resource }) -> ()
func testRegularClosureCannotConsumeCapturedStruct(r: consuming Resource) { // expected-error {{missing reinitialization of closure capture 'r' after consume}}
  let g = { // not @callable(once)
    r.use() // expected-note {{consumed here}}
  }
  g()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A38NestedClosurePropagatesConsumedCapture1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@owned Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A38NestedClosurePropagatesConsumedCapture1ryAA8ResourceVn_tFyyXOofU0_ : $@convention(thin) (@owned @callable(once) @callee_owned () -> ()) -> ()
func testNestedClosurePropagatesConsumedCapture(r: consuming Resource) {
  let inner = { @callable(once) in r.use() }
  let outer = { @callable(once) in inner() }
  outer()
}

// FIXME: [called-once] The closures only borrow `r`, and nothing escapes, so `r`
// should be promoted out of its box and this consume should be accepted.
// SILGEN-LABEL: sil private [ossa] @$s4test0A47CaptureNotConsumedWhenOnlyBorrowedAcrossNesting1ryAA8ResourceVn_tFyyXOofU_ : $@convention(thin) (@guaranteed { var Resource }) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A47CaptureNotConsumedWhenOnlyBorrowedAcrossNesting1ryAA8ResourceVn_tFyyXOofU0_ : $@convention(thin) (@owned @callable(once) @callee_owned () -> ()) -> ()
func testCaptureNotConsumedWhenOnlyBorrowedAcrossNesting(r: consuming Resource) {
  let inner = { @callable(once) in r.peek() }
  let outer = { @callable(once) in inner() }
  outer()
  r.use() // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A47ConsumingCalledAtMostOnceNestedInRegularClosure1ryAA8ResourceVn_tFyycfU_ : $@convention(thin) (@guaranteed { var Resource }) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A47ConsumingCalledAtMostOnceNestedInRegularClosure1ryAA8ResourceVn_tFyycfU_yyXOofU_ : $@convention(thin) (@owned Resource) -> ()
func testConsumingCalledAtMostOnceNestedInRegularClosure(r: consuming Resource) { // expected-error {{reinitialization of closure capture 'r' after consume}}
  let f = {
    let g = { @callable(once) in // expected-note {{consumed here}}
      r.use()
    }
    g()
  }
  f()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A30ConsumingGetterConsumesCaptureyyAA3BoxVnFyyXOofU_ : $@convention(thin) (@owned Box) -> ()
func testConsumingGetterConsumesCapture(_ box: consuming Box) {
  let g = { @callable(once) in
    let v = box.r
    _ = v
  }
  g()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A30ConsumingSetterConsumesCaptureyyAA4SlotVn_AA8ResourceVntFyyXOofU_ : $@convention(thin) (@owned Slot, @owned Resource) -> ()
func testConsumingSetterConsumesCapture(_ slot: consuming Slot, _ r: consuming Resource) {
  let g = { @callable(once) in
    slot.r = r
  }
  g()
}

protocol Usable: ~Copyable {
  consuming func use()
  func test()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A36GenericConsumingCaptureIsAddressOnlyyyxnAA6UsableRzRi_zlFyyXOofU_ : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> ()
func testGenericConsumingCaptureIsAddressOnly<T: Usable & ~Copyable>(_ t: consuming T) {
  let g = { @callable(once) in
    t.use()
  }
  g()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A44GenericConsumingCaptureIsAddressOnlyMultiUseyyxnAA6UsableRzRi_zlFyyXOofU_ : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> ()
func testGenericConsumingCaptureIsAddressOnlyMultiUse<T: Usable & ~Copyable>(_ t: consuming T) { // expected-error {{'t' used after consume}}
  let g = { @callable(once) in // expected-note {{consumed here}}
    t.use()
  }
  _ = g
  t.test() // expected-note {{used here}}
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A5CastsyyF0A12IdentityCastL_yAaByyF2NCL_VAEnFAEyXEfU_ : $@convention(thin) (@owned NC) -> @owned NC
// SILGEN-LABEL: sil private [ossa] @$s4test0A5CastsyyF0A5EraseL_yyAaByyF1SL_VnFAA6Usable_pRi_s_XPyXOofU_ : $@convention(thin) (@owned S) -> @out any Usable & ~Copyable
// SILGEN-LABEL: sil private [ossa] @$s4test0A5CastsyyF12genericEraseL_yyxnAA6UsableRzRi_zlFAaD_pRi_s_XPyXOofU_ : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> @out any Usable & ~Copyable
func testCasts() {
  struct NC: ~Copyable {}
  struct S: ~Copyable, Usable {
    consuming func use() {}
    func test() {}
  }

  func testIdentityCast(_ x: consuming NC) -> NC {
    { @callable(once) in x as NC }()
  }

  func testErase(_ c: consuming S) {
    let fn = { @callable(once) in c as any Usable & ~Copyable }
    _ = fn()
  }

  func genericErase<T: Usable & ~Copyable>(_ v: consuming T) {
    let fn = { @callable(once) in v as any Usable & ~Copyable }
    _ = fn()
  }
}

func calledAtMostOnce(_ fn: @callable(once) () -> Void) { fn() }

// An at-most-once closure can't consume a borrowed capture, just like any
// other closure.
// SILGEN-LABEL: sil private [ossa] @$s4test0A24ConsumingBorrowedCapture1ryAA8ResourceV_tFyyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A24ConsumingBorrowedCapture1ryAA8ResourceV_tFyyXEfU0_ : $@convention(thin) (@guaranteed Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A24ConsumingBorrowedCapture1ryAA8ResourceV_tFyyXEfU1_ : $@convention(thin) (@guaranteed Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A24ConsumingBorrowedCapture1ryAA8ResourceV_tFyyXEfU2_ : $@convention(thin) (@guaranteed Resource) -> ()
func testConsumingBorrowedCapture(r: borrowing Resource) { // expected-error 4 {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  func consumeResource(_: consuming Resource) {}

  calledAtMostOnce { _ = consume r } // expected-note {{consumed here}}
  calledAtMostOnce { consumeResource(r) } // expected-note {{consumed here}}
  calledAtMostOnce { r.use() } // expected-note {{consumed here}}
  calledAtMostOnce { _ = r as any ~Copyable } // expected-note {{consumed here}}
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A38ConsumingBorrowedCaptureInPlainClosure1ryAA8ResourceV_tFyyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
func testConsumingBorrowedCaptureInPlainClosure(r: borrowing Resource) { // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  func plain(_ fn: () -> Void) { fn() }
  plain { r.use() } // expected-note {{consumed here}}
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A32ConsumingClosureParameterCaptureyyFyAA8ResourceVcfU_ : $@convention(thin) (@guaranteed Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A32ConsumingClosureParameterCaptureyyFyAA8ResourceVcfU_yyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A32ConsumingClosureParameterCaptureyyFyAA8ResourceVncfU0_ : $@convention(thin) (@owned Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A32ConsumingClosureParameterCaptureyyFyAA8ResourceVncfU0_yyXEfU_ : $@convention(thin) (@owned Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A32ConsumingClosureParameterCaptureyyFyAA8ResourceVzcfU1_ : $@convention(thin) (@inout Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A32ConsumingClosureParameterCaptureyyFyAA8ResourceVzcfU1_yyXEfU_ : $@convention(thin) (@owned Resource) -> ()
func testConsumingClosureParameterCapture() {
  let _: (borrowing Resource) -> Void = { r in // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
    calledAtMostOnce { r.use() } // expected-note {{consumed here}}
  }
  let _: (consuming Resource) -> Void = { r in
    calledAtMostOnce { r.use() }
  }
  let _: (inout Resource) -> Void = { r in // expected-error {{missing reinitialization of inout parameter 'r' after consume}}
    calledAtMostOnce { r.use() } // expected-note {{consumed here}}
  }
}

extension Resource {
  // SILGEN-LABEL: sil private [ossa] @$s4test8ResourceV0A12BorrowedSelfyyFyyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
  func testBorrowedSelf() {
    calledAtMostOnce { self.peek() }
  }

  // SILGEN-LABEL: sil private [ossa] @$s4test8ResourceV0A21ConsumingBorrowedSelfyyFyyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
  func testConsumingBorrowedSelf() { // expected-error {{non-'Copyable' 'self' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
    calledAtMostOnce { self.use() } // expected-note {{consumed here}}
  }

  // SILGEN-LABEL: sil private [ossa] @$s4test8ResourceV0A21ConsumingConsumedSelfyyFyyXEfU_ : $@convention(thin) (@owned Resource) -> ()
  consuming func testConsumingConsumedSelf() {
    calledAtMostOnce { self.use() }
  }

  // SILGEN-LABEL: sil private [ossa] @$s4test8ResourceV0A20ConsumingMutatedSelfyyFyyXEfU_ : $@convention(thin) (@owned Resource) -> ()
  mutating func testConsumingMutatedSelf() { // expected-error {{missing reinitialization of inout parameter 'self' after consume}}
    calledAtMostOnce { self.use() } // expected-note {{consumed here}}
  }

  // SILGEN-LABEL: sil private [ossa] @$s4test8ResourceV0A12MutatingSelfyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Resource) -> ()
  mutating func testMutatingSelf() {
    calledAtMostOnce { self.change() }
  }
}

// A setter owns its `newValue`, so an at-most-once closure can consume it.
// SILGEN-LABEL: sil private [ossa] @$s4test21SlotConsumingNewValueV1rAA8ResourceVvsyyXEfU_ : $@convention(thin) (@owned Resource) -> ()
struct SlotConsumingNewValue: ~Copyable {
  var r: Resource {
    get { Resource() }
    set { calledAtMostOnce { newValue.use() } }
  }
}

// A non-mutating setter borrows `self`, so an at-most-once closure can't
// consume it.
// SILGEN-LABEL: sil private [ossa] @$s4test17SlotBorrowingSelfV5valueSivsyyXEfU_ : $@convention(thin) (@guaranteed SlotBorrowingSelf) -> ()
struct SlotBorrowingSelf: ~Copyable {
  var value: Int {
    get { 0 }
    nonmutating set { // expected-error {{non-'Copyable' 'self' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
      calledAtMostOnce { self.consume() } // expected-note {{consumed here}}
    }
  }
  consuming func consume() {}
}

// A non-mutating modify accessor borrows `self`.
// SILGEN-LABEL: sil private [ossa] @$s4test19ModifyBorrowingSelfV5valueSivMyyXEfU_ : $@convention(thin) (@guaranteed ModifyBorrowingSelf) -> ()
struct ModifyBorrowingSelf: ~Copyable {
  var value: Int {
    get { 0 }
    nonmutating _modify { // expected-error {{non-'Copyable' 'self' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
      calledAtMostOnce { self.consume() } // expected-note {{consumed here}}
      var value = 0
      yield &value
    }
  }
  consuming func consume() {}
}

// An init accessor owns its `initialValue`, so an at-most-once closure can
// consume it.
// SILGEN-LABEL: sil private [ossa] @$s4test26InitAccessorConsumingValueV1rAA8ResourceVviyyXEfU_ : $@convention(thin) (@owned Resource) -> ()
struct InitAccessorConsumingValue: ~Copyable {
  var _stored: Int
  var r: Resource {
    @storageRestrictions(initializes: _stored)
    init(initialValue) {
      calledAtMostOnce { initialValue.use() }
      _stored = 0
    }
    get { Resource() }
  }
  init() { r = Resource() }
}

func consumeValue<T: ~Copyable>(_ t: consuming T) {}
func plain(_ fn: () -> Void) { fn() }
func escaping(_ fn: @escaping () -> Void) { fn() }
func makeValue<T: ~Copyable>() -> T { fatalError() }

// A `borrowing` binding is borrowed.
// SILGEN-LABEL: sil private [ossa] @$s4test0A32ConsumingBorrowingPatternBindingyyAA8ResourceVSgFyyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
func testConsumingBorrowingPatternBinding(_ e: borrowing Resource?) {
  switch e {
  case .some(borrowing r): // expected-warning {{'borrowing' in pattern matches is deprecated; use 'let'}}
    // expected-error@-1 {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
    calledAtMostOnce { r.use() } // expected-note {{consumed here}}
  case .none:
    break
  }
}

// A closure or function that can run more than once can't give away a `let`
// that it captures, so an at-most-once closure inside it can't consume one.
// SILGEN-LABEL: sil private [ossa] @$s4test0A34ConsumingLetCapturedByPlainClosureyyFyyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A34ConsumingLetCapturedByPlainClosureyyFyyXEfU_yyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
func testConsumingLetCapturedByPlainClosure() {
  let r = Resource() // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  plain {
    calledAtMostOnce { r.use() } // expected-note {{consumed here}}
  }
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A37ConsumingLetCapturedByEscapingClosureyyFyycfU_ : $@convention(thin) (@guaranteed { let Resource }) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A37ConsumingLetCapturedByEscapingClosureyyFyycfU_yyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
func testConsumingLetCapturedByEscapingClosure() {
  let r = Resource() // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  escaping {
    calledAtMostOnce { r.use() } // expected-note {{consumed here}}
  }
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A35ConsumingLetCapturedByLocalFunctionyyF5localL_yyFyyXEfU_ : $@convention(thin) (@guaranteed Resource) -> ()
func testConsumingLetCapturedByLocalFunction() {
  let r = Resource() // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  func local() {
    calledAtMostOnce { r.use() } // expected-note {{consumed here}}
  }
  local()
  local()
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A45ConsumingAddressOnlyLetCapturedByPlainClosureyyxmRi_zlFyyXEfU_ : $@convention(thin) <T where T : ~Copyable> (@in_guaranteed T) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A45ConsumingAddressOnlyLetCapturedByPlainClosureyyxmRi_zlFyyXEfU_yyXEfU_ : $@convention(thin) <T where T : ~Copyable> (@in_guaranteed T) -> ()
func testConsumingAddressOnlyLetCapturedByPlainClosure<T: ~Copyable>(
  _: T.Type
) {
  let t: T = makeValue()
  plain {
    calledAtMostOnce { consumeValue(t) } // expected-error {{'t' is borrowed and cannot be consumed}} expected-note {{consumed here}}
  }
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A44ConsumingSwitchBindingCapturedByPlainClosureyyxSgRi_zlFyyXEfU_ : $@convention(thin) <T where T : ~Copyable> (@in_guaranteed T) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A44ConsumingSwitchBindingCapturedByPlainClosureyyxSgRi_zlFyyXEfU_yyXEfU_ : $@convention(thin) <T where T : ~Copyable> (@in_guaranteed T) -> ()
func testConsumingSwitchBindingCapturedByPlainClosure<T: ~Copyable>(
  _ e: borrowing T?
) {
  switch e {
  case .some(let t):
    plain {
      calledAtMostOnce { consumeValue(t) } // expected-error {{'t' is borrowed and cannot be consumed}} expected-note {{consumed here}}
    }
  case .none:
    break
  }
}

// An enclosing at-most-once closure owns the captures that it consumes, so it
// can give them away.
// SILGEN-LABEL: sil private [ossa] @$s4test0A39ConsumingLetCapturedByAtMostOnceClosureyyFyyXEfU_ : $@convention(thin) (@owned Resource) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A39ConsumingLetCapturedByAtMostOnceClosureyyFyyXEfU_yyXEfU_ : $@convention(thin) (@owned Resource) -> ()
func testConsumingLetCapturedByAtMostOnceClosure() {
  let r = Resource()
  calledAtMostOnce {
    calledAtMostOnce { r.use() }
  }
}

// Calling a captured at-most-once function consumes it, so it can't be called
// through a closure or function that can run more than once either.
// SILGEN-LABEL: sil private [ossa] @$s4test0A35CallingLetCapturedByEscapingClosureyyFyyXOofU_ : $@convention(thin) () -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A35CallingLetCapturedByEscapingClosureyyFyycfU0_ : $@convention(thin) (@guaranteed { let @callable(once) @callee_owned () -> () }) -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A35CallingLetCapturedByEscapingClosureyyFyycfU0_yyXEfU_ : $@convention(thin) (@guaranteed @callable(once) @callee_owned () -> ()) -> ()
func testCallingLetCapturedByEscapingClosure() {
  let g = { @callable(once) in } // expected-error {{non-'Copyable' 'g' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  escaping {
    calledAtMostOnce { g() } // expected-note {{consumed here}}
  }
}

// SILGEN-LABEL: sil private [ossa] @$s4test0A33CallingLetCapturedByLocalFunctionyyFyyXOofU_ : $@convention(thin) () -> ()
// SILGEN-LABEL: sil private [ossa] @$s4test0A33CallingLetCapturedByLocalFunctionyyF5localL_yyFyyXEfU_ : $@convention(thin) (@guaranteed @callable(once) @callee_owned () -> ()) -> ()
func testCallingLetCapturedByLocalFunction() {
  let g = { @callable(once) in } // expected-error {{non-'Copyable' 'g' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  func local() {
    calledAtMostOnce { g() } // expected-note {{consumed here}}
  }
  local()
  local()
}

#if NONCOPYABLE_CASTING
protocol Shape: ~Copyable {}
struct Circle: Shape, ~Copyable {}
struct Square: Shape, ~Copyable {}

func andAtMostOnce(
  _ lhs: Bool, _ rhs: @autoclosure @callable(once) () -> Bool
) -> Bool {
  lhs ? rhs() : false
}

// An `is` check only borrows its operand, so the autoclosure doesn't consume
// the capture.
// SILGEN-CASTING-LABEL: sil private [transparent] [ossa] @$s4test0A28IsCheckDoesNotConsumeCaptureyyAA5Shape_pRi_s_XPnFSbyXEfu_ : $@convention(thin) (@inout_aliasable any Shape & ~Copyable) -> Bool
func testIsCheckDoesNotConsumeCapture(_ shape: consuming any Shape & ~Copyable) {
  _ = andAtMostOnce(shape is Circle, shape is Square)
  _ = consume shape
}

// SILGEN-CASTING-LABEL: sil private [transparent] [ossa] @$s4test0A24IsCheckOfBorrowedCaptureyyAA5Shape_pRi_s_XPFSbyXEfu_ : $@convention(thin) (@in_guaranteed any Shape & ~Copyable) -> Bool
func testIsCheckOfBorrowedCapture(_ shape: borrowing any Shape & ~Copyable) {
  _ = andAtMostOnce(shape is Circle, shape is Square)
}

// A function type argument doesn't affect the representation of the struct, so
// an `is` check of the struct still only borrows it.
struct NCBox<T>: ~Copyable {
  var t: T
}

// SILGEN-CASTING-LABEL: sil private [transparent] [ossa] @$s4test0a20IsCheckOfBorrowedBoxD8FunctionyyAA5NCBoxVyyycGFSbyXEfu_ : $@convention(thin) (@guaranteed NCBox<() -> ()>) -> Bool
func testIsCheckOfBorrowedBoxOfFunction(_ b: borrowing NCBox<() -> Void>) {
  _ = andAtMostOnce(true, b is Int)
  // expected-casting-warning@-1 {{cast from 'NCBox<() -> Void>' to unrelated type 'Int' always fails}}
}

// SILGEN-CASTING-LABEL: sil private [transparent] [ossa] @$s4test0a12IsCheckOfBoxD29FunctionDoesNotConsumeCaptureyyAA5NCBoxVyyycGnFSbyXEfu_ : $@convention(thin) (@inout_aliasable NCBox<() -> ()>) -> Bool
func testIsCheckOfBoxOfFunctionDoesNotConsumeCapture(
  _ b: consuming NCBox<() -> Void>
) {
  _ = andAtMostOnce(true, b is Int)
  // expected-casting-warning@-1 {{cast from 'NCBox<() -> Void>' to unrelated type 'Int' always fails}}
  _ = consume b
}

// An `is` check of a function consumes it, so the autoclosure does too.
// SILGEN-CASTING-LABEL: sil private [transparent] [ossa] @$s4test0A32IsCheckOfFunctionConsumesCaptureyyyyXEnFSbyXEfu_ : $@convention(thin) (@owned @noescape @callable(once) @callee_owned () -> ()) -> Bool
func testIsCheckOfFunctionConsumesCapture(_ body: @callable(once) () -> Void) {
  _ = andAtMostOnce(true, body is Int)
  // expected-casting-warning@-1 {{cast from '@callable(once) () -> Void' to unrelated type 'Int' always fails}}
}

// SILGEN-CASTING-LABEL: sil private [transparent] [ossa] @$s4test0A25UseAfterIsCheckOfFunctionyyyyXEnFSbyXEfu_ : $@convention(thin) (@owned @noescape @callable(once) @callee_owned () -> ()) -> Bool
func testUseAfterIsCheckOfFunction(_ body: @callable(once) () -> Void) {
  // expected-casting-error@-1 {{'body' consumed more than once}}
  _ = andAtMostOnce(true, body is Int) // expected-casting-note {{consumed here}}
  // expected-casting-warning@-1 {{cast from '@callable(once) () -> Void' to unrelated type 'Int' always fails}}
  body() // expected-casting-note {{consumed again here}}
}

// An `is` check of an optional only borrows its operand too
// SILGEN-CASTING-LABEL: sil private [transparent] [ossa] @$s4test0A38IsCheckOfOptionalDoesNotConsumeCaptureyyAA5Shape_pRi_s_XPSgnFSbyXEfu_ : $@convention(thin) (@inout_aliasable Optional<any Shape & ~Copyable>) -> Bool
func testIsCheckOfOptionalDoesNotConsumeCapture(
  _ shape: consuming (any Shape & ~Copyable)?
) {
  _ = andAtMostOnce(true, shape is Circle)
  _ = consume shape
}
#endif
