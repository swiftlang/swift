// RUN: %target-swift-frontend %s \
// RUN: -emit-sil \
// RUN: -enable-experimental-feature OnceFunctions \
// RUN: -verify

// REQUIRES: swift_feature_OnceFunctions

struct Resource: ~Copyable {
  init() {}
  consuming func use() {}
  borrowing func peek() {}
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
  let g = { @once in
    let taken = consume r
    taken.use()
  }
  g()
}

// Source of an assignment.
func testAssignmentConsumesCapturedStruct(r: consuming Resource) {
  let g = { @once in
    var local = Resource()
    local = r
    local.use()
  }
  g()
}

// Passing to a `consuming` parameter of a plain function.
func testConsumingParamCallConsumesCapture(r: consuming Resource) {
  func consumeResource(_: consuming Resource) {}

  let g = { @once in
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

  let g = { @once in
    let w = Wrapper(r: r)
    _ = w
  }
  g()

  let localResource = Resource()
  let h = { @once in
    let w = ConsumingWrapper(localResource)
    _ = w
  }
  h()
}

// Calling a `consuming` method (`self` is consumed).
func testConsumingMethodCallConsumesCapture(r: consuming Resource) {
  let g = { @once in
    r.use()
  }
  g()
}

// Intiailization consumes the value
func testLocalBindingConsumesCapture(r: consuming Resource) {
  let g = { @once in
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

  let g = { @once in
    s.prop = r1
  }
  g()

  let h = { @once in
    c.prop = r2
  }
  h()

  // Make sure that the same value cannot be consumed twice by different closures

  let r3 = Resource() // expected-error {{'r3' consumed more than once}}
  let _ = { @once in // expected-note {{consumed here}}
    s.prop = r3
  }

  let _ = { @once in // expected-note {{consumed again here}}
    c.prop = r3
  }
}

// Capture aliasing.
func testCaptureListConsumesCapture(r: consuming Resource) {
  let g = { @once [taken = r] in
    taken.use()
  }
  g()
}

// `return` is a consuming use.
func testReturnConsumesCapture(r: consuming Resource) -> Resource {
  let g: @once () -> Resource = { @once in
    return r
  }
  return g()
}

func testBorrowingCallDoesNotConsumeCapture(r: consuming Resource) {
  func borrowResource(_: borrowing Resource) {}

  let g = { @once in
    r.peek() // borrowing use
    borrowResource(r)
    r.use() // consume
  }
  g()
}

func testDoubleConsumeOfCapturedValue(r: consuming Resource) { // expected-error 2 {{'r' consumed more than once}}
  let g = { @once in
    r.use() // expected-note 2 {{consumed here}}
    r.use() // expected-note 2 {{consumed again here}}
  }
  g()
}

func testRegularClosureCannotConsumeCapturedStruct(r: consuming Resource) { // expected-error {{missing reinitialization of closure capture 'r' after consume}}
  let g = { // not exactly-once
    r.use() // expected-note {{consumed here}}
  }
  g()
}

func testNestedClosurePropagatesConsumedCapture(r: consuming Resource) {
  let inner = { @once in r.use() }
  let outer = { @once in inner() }
  outer()
}

// FIXME: [called-once] The closures only borrow `r`, and nothing escapes, so `r`
// should be promoted out of its box and this consume should be accepted.
func testCaptureNotConsumedWhenOnlyBorrowedAcrossNesting(r: consuming Resource) {
  let inner = { @once in r.peek() }
  let outer = { @once in inner() }
  outer()
  r.use() // expected-error {{non-'Copyable' 'r' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
}

func testConsumingCalledExactlyOnceNestedInRegularClosure(r: consuming Resource) { // expected-error {{reinitialization of closure capture 'r' after consume}}
  let f = {
    let g = { @once in // expected-note {{consumed here}}
      r.use()
    }
    g()
  }
  f()
}

func testConsumingGetterConsumesCapture(_ box: consuming Box) {
  let g = { @once in
    let v = box.r
    _ = v
  }
  g()
}

func testConsumingSetterConsumesCapture(_ slot: consuming Slot, _ r: consuming Resource) {
  let g = { @once in
    slot.r = r
  }
  g()
}

protocol Usable: ~Copyable {
  consuming func use()
  func test()
}

func testGenericConsumingCaptureIsAddressOnly<T: Usable & ~Copyable>(_ t: consuming T) {
  let g = { @once in
    t.use()
  }
  g()
}

func testGenericConsumingCaptureIsAddressOnlyMultiUse<T: Usable & ~Copyable>(_ t: consuming T) { // expected-error {{'t' used after consume}}
  let g = { @once in // expected-note {{consumed here}}
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
    { @once in x as NC }()
  }

  func testErase(_ c: consuming S) {
    let fn = { @once in c as any Usable & ~Copyable }
    _ = fn()
  }

  func genericErase<T: Usable & ~Copyable>(_ v: consuming T) {
    let fn = { @once in v as any Usable & ~Copyable }
    _ = fn()
  }
}
