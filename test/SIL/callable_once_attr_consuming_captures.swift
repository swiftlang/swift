// RUN: %target-swift-frontend %s \
// RUN: -emit-sil \
// RUN: -enable-experimental-feature OnceFunctions \
// RUN: -verify

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
func testExplicitConsumeOfCapturedStruct(r: consuming Resource) {
  let g = { @callable(once) in
    let taken = consume r
    taken.use()
  }
  g()
}

// Source of an assignment.
func testAssignmentConsumesCapturedStruct(r: consuming Resource) {
  let g = { @callable(once) in
    var local = Resource()
    local = r
    local.use()
  }
  g()
}

// Passing to a `consuming` parameter of a plain function.
func testConsumingParamCallConsumesCapture(r: consuming Resource) {
  func consumeResource(_: consuming Resource) {}

  let g = { @callable(once) in
    consumeResource(r)
  }
  g()
}

// Passing to an initializer's implicitly-`consuming` parameter.
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
func testConsumingMethodCallConsumesCapture(r: consuming Resource) {
  let g = { @callable(once) in
    r.use()
  }
  g()
}

// Intiailization consumes the value
func testLocalBindingConsumesCapture(r: consuming Resource) {
  let g = { @callable(once) in
    let taken = r
    taken.use()
  }
  g()
}

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
func testCaptureListConsumesCapture(r: consuming Resource) {
  let g = { @callable(once) [taken = r] in
    taken.use()
  }
  g()
}

// `return` is a consuming use.
func testReturnConsumesCapture(r: consuming Resource) -> Resource {
  let g: @callable(once) () -> Resource = { @callable(once) in
    return r
  }
  return g()
}

func testBorrowingCallDoesNotConsumeCapture(r: consuming Resource) {
  func borrowResource(_: borrowing Resource) {}

  let g = { @callable(once) in
    r.peek() // borrowing use
    borrowResource(r)
    r.use() // consume
  }
  g()
}

func testDoubleConsumeOfCapturedValue(r: consuming Resource) { // expected-error 2 {{'r' consumed more than once}}
  let g = { @callable(once) in
    r.use() // expected-note 2 {{consumed here}}
    r.use() // expected-note 2 {{consumed again here}}
  }
  g()
}

func testRegularClosureCannotConsumeCapturedStruct(r: consuming Resource) { // expected-error {{missing reinitialization of closure capture 'r' after consume}}
  let g = { // not @callable(once)
    r.use() // expected-note {{consumed here}}
  }
  g()
}

func testNestedClosurePropagatesConsumedCapture(r: consuming Resource) {
  let inner = { @callable(once) in r.use() }
  let outer = { @callable(once) in inner() }
  outer()
}

// FIXME: [called-once] The closures only borrow `r`, and nothing escapes, so `r`
// should be promoted out of its box and this consume should be accepted.
func testCaptureNotConsumedWhenOnlyBorrowedAcrossNesting(r: consuming Resource) {
  let inner = { @callable(once) in r.peek() }
  let outer = { @callable(once) in inner() }
  outer()
  r.use() // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
}

func testConsumingCalledAtMostOnceNestedInRegularClosure(r: consuming Resource) { // expected-error {{reinitialization of closure capture 'r' after consume}}
  let f = {
    let g = { @callable(once) in // expected-note {{consumed here}}
      r.use()
    }
    g()
  }
  f()
}

func testConsumingGetterConsumesCapture(_ box: consuming Box) {
  let g = { @callable(once) in
    let v = box.r
    _ = v
  }
  g()
}

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

func testGenericConsumingCaptureIsAddressOnly<T: Usable & ~Copyable>(_ t: consuming T) {
  let g = { @callable(once) in
    t.use()
  }
  g()
}

func testGenericConsumingCaptureIsAddressOnlyMultiUse<T: Usable & ~Copyable>(_ t: consuming T) { // expected-error {{'t' used after consume}}
  let g = { @callable(once) in // expected-note {{consumed here}}
    t.use()
  }
  _ = g
  t.test() // expected-note {{used here}}
}

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
func testConsumingBorrowedCapture(r: borrowing Resource) { // expected-error 4 {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  func consumeResource(_: consuming Resource) {}

  calledAtMostOnce { _ = consume r } // expected-note {{consumed here}}
  calledAtMostOnce { consumeResource(r) } // expected-note {{consumed here}}
  calledAtMostOnce { r.use() } // expected-note {{consumed here}}
  calledAtMostOnce { _ = r as any ~Copyable } // expected-note {{consumed here}}
}

func testConsumingBorrowedCaptureInPlainClosure(r: borrowing Resource) { // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  func plain(_ fn: () -> Void) { fn() }
  plain { r.use() } // expected-note {{consumed here}}
}

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
  func testBorrowedSelf() {
    calledAtMostOnce { self.peek() }
  }

  func testConsumingBorrowedSelf() { // expected-error {{non-'Copyable' 'self' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
    calledAtMostOnce { self.use() } // expected-note {{consumed here}}
  }

  consuming func testConsumingConsumedSelf() {
    calledAtMostOnce { self.use() }
  }

  mutating func testConsumingMutatedSelf() { // expected-error {{missing reinitialization of inout parameter 'self' after consume}}
    calledAtMostOnce { self.use() } // expected-note {{consumed here}}
  }

  mutating func testMutatingSelf() {
    calledAtMostOnce { self.change() }
  }
}

// A setter owns its `newValue`, so an at-most-once closure can consume it.
struct SlotConsumingNewValue: ~Copyable {
  var r: Resource {
    get { Resource() }
    set { calledAtMostOnce { newValue.use() } }
  }
}

// A non-mutating setter borrows `self`, so an at-most-once closure can't
// consume it.
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
