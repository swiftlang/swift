// RUN: %target-swift-frontend -emit-sil -verify %s -o /dev/null -enable-experimental-feature NondeinitableTypes -enable-experimental-feature OnceFunctions

// REQUIRES: swift_feature_NondeinitableTypes
// REQUIRES: swift_feature_OnceFunctions

// FIXME: [deinitable] The synthesized setter of `x` overwrites the only stored property, which the move-only checker
// handles by destroying the whole struct. See "Partial reinitialization" below.
struct ND: ~Copyable, ~Deinitable {
  var x: Int
  // expected-error@-1 {{'self' is not consumed on all paths}}
  // expected-note@-2 {{path exits here without a consume}}
  consuming func finish() { discard self }
  mutating func bump() { x += 1 }
}

enum Choice: ~Copyable, ~Deinitable {
  case a(Int), b
  consuming func finish() { discard self }
}

// FIXME: [deinitable] The synthesized setter of a stored property drops its old value. The error should name 'self.nd'.
struct Holder: ~Copyable, ~Deinitable {
  var nd: ND
  // expected-error@-1 {{'self' is not consumed on all paths}}
  // expected-note@-2 {{path exits here without a consume}}
}

struct Box<T: ~Copyable & ~Deinitable>: ~Copyable, ~Deinitable {
  let t: T
  consuming func take() -> T { t }
}

func make() -> ND { ND(x: 0) }
func mayThrow() throws {}
func transfer(_ nd: consuming ND) -> ND { nd }

// MARK: - Values that aren't consumed on every path

func neverConsumed() {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  _ = nd.x
} // expected-note {{path exits here without a consume}}

func consumedOnOneBranch(_ b: Bool) {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  if b { nd.finish() } // expected-note {{path exits here without a consume}}
}

func dropped() {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  // FIXME: [deinitable] This diagnostic is confusing when paired with the 'consume' operator
  _ = consume nd // expected-note {{path exits here without a consume}}
}

func varOverwrite() {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  nd = make() // expected-note {{path exits here without a consume}}
  nd.finish()
}

func inoutOverwrite(_ nd: inout ND) { // expected-error {{'nd' is not consumed on all paths}}
  nd = make() // expected-note {{path exits here without a consume}}
}

// FIXME: [deinitable] The error should name 'h.nd'.
func storedPropertyOverwrite(_ h: inout Holder) { // expected-error {{'h' is not consumed on all paths}}
  h.nd = make() // expected-note {{path exits here without a consume}}
}

func throwingPath() throws {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  try mayThrow() // expected-note {{path exits here without a consume}}
  nd.finish()
}

func genericParameter<T: ~Copyable & ~Deinitable>(_ t: consuming T) {} // expected-error {{'t' is not consumed on all paths}}
// expected-note@-1 {{path exits here without a consume}}

func genericContainer(_ b: consuming Box<ND>) {} // expected-error {{'b' is not consumed on all paths}}
// expected-note@-1 {{path exits here without a consume}}

func switchDropsPayload(_ c: consuming Choice) { // expected-error {{'c' is not consumed on all paths}}
  switch consume c { // expected-note {{path exits here without a consume}}
  case .a: break
  case .b: break
  }
}

func escapingCaptureNotConsumed() {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  let f = { nd.x += 1 }
  f()
} // expected-note {{path exits here without a consume}}

var escapedClosure: (() -> Void)? = nil

func capturedInBoxByEscapingClosure() {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  escapedClosure = { nd.x += 1 }
} // expected-note {{path exits here without a consume}}

func capturedInBoxByLocalFunction() {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  func local() { _ = nd.x }
  local()
} // expected-note {{path exits here without a consume}}

func dropsInInfiniteLoop() -> Never {
  while true {
    let nd = make() // expected-error {{'nd' is not consumed on all paths}}
    _ = nd.x
  }
}

func overwritesInInfiniteLoop() {
  while true {
    var nd = make() // expected-error {{'nd' is not consumed on all paths}}
    nd = make() // expected-note {{path exits here without a consume}}
    nd.finish()
  }
}

// MARK: - Accepted

func transfers(_ nd: consuming ND) -> ND { nd }

func finishes() { make().finish() }

func consumedOnEveryBranch(_ b: Bool) {
  let nd = make()
  if b { nd.finish() } else { transfer(nd).finish() }
}

func reinitialized() {
  var nd = make()
  nd.finish()
  nd = make()
  nd.finish()
}

func traps() {
  let nd = make()
  _ = nd.x
  fatalError()
}

func trapsOnOneBranch(_ b: Bool) {
  let nd = make()
  if b { fatalError() }
  nd.finish()
}

func unwrapsGenericContainer(_ b: consuming Box<ND>) { b.take().finish() }

func finishesEnum(_ c: consuming Choice) { c.finish() }

func escapingCapture() {
  var nd = make()
  let f = { nd.x += 1 }
  f()
  nd.finish()
}

func loop(_ n: Int) {
  for _ in 0..<n {
    let nd = make()
    nd.finish()
  }
}

// MARK: - Partial reinitialization

// FIXME: [deinitable] The move-only checker tracks the liveness of each stored property separately. A struct with a
// single stored property has a single leaf, so overwriting that property reinitializes every leaf, and the checker
// destroys the whole struct beforehand even though only the old value of the property is dropped. The destroy should be
// of the overwritten property instead. Until then, these in-place mutations are diagnosed as if the whole value were
// dropped.

struct Pair: ~Copyable, ~Deinitable {
  var x: Int
  var y: Int
  consuming func finish() { discard self }
}

struct Outer: ~Copyable, ~Deinitable {
  var nd: ND
  // expected-error@-1 {{'self' is not consumed on all paths}}
  // expected-note@-2 {{path exits here without a consume}}
}

func mutatesInPlace() {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  nd.bump() // expected-note {{path exits here without a consume}}
  nd.x = 3
  nd.finish()
}

func reinitializesEveryProperty() {
  var p = Pair(x: 1, y: 2) // expected-error {{'p' is not consumed on all paths}}
  p.x = 3 // expected-note {{path exits here without a consume}}
  p.y = 4
  p.finish()
}

func reinitializesInBranch(_ b: Bool) {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  if b { nd.x = 3 } // expected-note {{path exits here without a consume}}
  nd.finish()
}

func reinitializesInLoop(_ n: Int) {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  for i in 0..<n { nd.x = i } // expected-note {{path exits here without a consume}}
  nd.finish()
}

func reinitializesAfterRead() {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  _ = nd.x // expected-note {{path exits here without a consume}}
  nd.x = 5
  nd.finish()
}

func reinitializesInout(_ nd: inout ND) { // expected-error {{'nd' is not consumed on all paths}}
  nd.x = 9 // expected-note {{path exits here without a consume}}
}

func reinitializesThroughNestedProperties(_ o: inout Outer) { // expected-error {{'o' is not consumed on all paths}}
  o.nd.x = 9 // expected-note {{path exits here without a consume}}
}

// A stored property that isn't overwritten stays alive, so the destroy doesn't include it.
func reinitializesOneOfTwoProperties() {
  var p = Pair(x: 1, y: 2)
  p.x = 3
  p.finish()
}

struct Sibling: ~Copyable, ~Deinitable {
  var a: Int
  let nd: ND
}

func dropsSibling() {
  var s = Sibling(a: 1, nd: make()) // expected-error {{'s' is not consumed on all paths}}
  s.a = 5
  _ = s.a
} // expected-note {{path exits here without a consume}}

// MARK: - Captures that are consumed

// FIXME: [deinitable] The move-only checker marks a function that it diagnosed, so that the implicit destroy checker
// skips it, but a closure and a 'defer' body are move-checked after the function that contains them. A mistake in what
// they capture is therefore also reported as the value not being consumed in the enclosing function. Each "is not
// consumed" error in this section is redundant.

func consumedByDefer() {
  let nd = make() // expected-error {{non-'Copyable' 'nd' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  // expected-error@-1 {{'nd' is not consumed on all paths}}
  defer { nd.finish() } // expected-note {{consumed here}}
  print("body")
} // expected-note {{path exits here without a consume}}

func consumedByEscapingClosure() {
  let nd = make() // expected-error {{non-'Copyable' 'nd' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  // expected-error@-1 {{'nd' is not consumed on all paths}}
  let f = { nd.finish() } // expected-note {{consumed here}}
  f()
} // expected-note {{path exits here without a consume}}

func consumedByLocalFunction() {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  func local() {
    nd.finish() // expected-error {{non-'Copyable' 'nd' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  }
  local()
} // expected-note {{path exits here without a consume}}

// MARK: - Witness thunks

// FIXME: [deinitable] A witness whose parameter is 'borrowing' can satisfy a requirement whose parameter is 'consuming',
// because the witness thunk destroys the argument after the call. Sema should reject the witness instead, and say why.
protocol TakesConsuming {
  func take(_ nd: consuming ND)
}

struct BorrowingWitness: TakesConsuming {
  func take(_ nd: borrowing ND) {} // expected-error {{value of type 'ND' is not consumed on all paths}}
  // expected-note@-1 {{path exits here without a consume}}
}

struct ConsumingWitness: TakesConsuming {
  func take(_ nd: consuming ND) { nd.finish() }
}

// A parameter that is only noncopyable can be dropped by the thunk.
protocol TakesNoncopyable<A> {
  associatedtype A: ~Copyable
  func take(_ a: consuming A)
}

struct NoncopyableWitness<X: ~Copyable>: TakesNoncopyable {
  typealias A = X
  func take(_ a: borrowing X) {}
}

// MARK: - Stored properties that are exactly-once functions

struct Payload: ~Copyable, ~Deinitable {
  let x: Int
}

struct HoldsCallback: ~Copyable, ~Deinitable {
  let callback: @once () -> Void
  let payload: Payload

  // The dropped stored property is an exactly-once function, so it has to be called.
  consuming func takePayload() -> Payload { payload } // expected-error {{'self.callback' is not called on all paths}}
  // expected-note@-1 {{path exits here without a call}}

  consuming func callAndTakePayload() -> Payload {
    callback()
    return payload
  }
}

// MARK: - Switching over enums

// FIXME: [deinitable] `switch consume` consumes the enum, but SILGen also destroys the enum in each case that doesn't bind
// a payload. A switch is therefore only accepted if every case binds its payload. An enum that has a case without a
// payload can't be consumed by a switch at all, and `discard self` isn't supported yet for an enum that has a payload
// that can't be destroyed implicitly. `if case` and `guard case` aren't affected, so they are accepted.
enum TwoInts: ~Copyable, ~Deinitable {
  case a(Int), b(Int)
}

enum TwoNDs: ~Copyable, ~Deinitable {
  case first(ND), second(ND)
}

// `discard self` destroys the payload, which is fine when the payload can be destroyed implicitly.
enum Named: ~Copyable, ~Deinitable {
  case name(String), unnamed

  consuming func finish() { discard self }
}

enum MaybeND: ~Copyable, ~Deinitable {
  case some(ND), none

  consuming func finish() { discard self } // expected-error {{'discard' is not yet supported for nondeinitable type 'MaybeND'}}
}

func switchBindsEveryPayload(_ t: consuming TwoInts) {
  switch consume t {
  case .a(let x): _ = x
  case .b(let y): _ = y
  }
}

func switchConsumesEveryNondeinitablePayload(_ t: consuming TwoNDs) {
  switch consume t {
  case .first(let nd): nd.finish()
  case .second(let nd): nd.finish()
  }
}

func switchIgnoresOnePayload(_ t: consuming TwoInts) { // expected-error {{'t' is not consumed on all paths}}
  switch consume t {
  case .a(let x): _ = x
  case .b: break
  } // expected-note {{path exits here without a consume}}
}

func switchOverEnumWithCaseWithoutPayload(_ m: consuming MaybeND) { // expected-error {{'m' is not consumed on all paths}}
  switch consume m {
  case .some(let nd): nd.finish()
  case .none: break
  } // expected-note {{path exits here without a consume}}
}

func ifCaseWithElse(_ m: consuming MaybeND) {
  if case .some(let nd) = consume m { nd.finish() } else { }
}

func guardCase(_ m: consuming MaybeND) {
  guard case .some(let nd) = consume m else { return }
  nd.finish()
}

// MARK: - Synthesized setters

// FIXME: [deinitable] The synthesized setter of a stored `var` overwrites the property. A struct that has only one
// stored property has only one leaf for the move-only checker to track, so it destroys the whole struct, and the setter
// is diagnosed as if it dropped the struct.
struct LetOnly: ~Copyable, ~Deinitable {
  let x: Int
}

struct TrivialVar: ~Copyable, ~Deinitable {
  var x: Int
  // expected-error@-1 {{'self' is not consumed on all paths}}
  // expected-note@-2 {{path exits here without a consume}}
}

struct StringVar: ~Copyable, ~Deinitable {
  var s: String
  // expected-error@-1 {{'self' is not consumed on all paths}}
  // expected-note@-2 {{path exits here without a consume}}
}

struct PrivateSetter: ~Copyable, ~Deinitable {
  private(set) var x: Int
  // expected-error@-1 {{'self' is not consumed on all paths}}
  // expected-note@-2 {{path exits here without a consume}}
  init() { x = 0 }
}

struct TwoTrivialVars: ~Copyable, ~Deinitable {
  var x: Int
  var y: Int
}

struct ExplicitSetter: ~Copyable, ~Deinitable {
  let base: Int
  var doubled: Int {
    get { base * 2 }
    set { }
  }
}

// MARK: - Generic values

func returnsGeneric<T: ~Copyable & ~Deinitable>(_ t: consuming T) -> T { t }

func forwardsGeneric<T: ~Copyable & ~Deinitable>(_ t: consuming T) -> T {
  returnsGeneric(t)
}

func consumesGenericOnEveryBranch<T: ~Copyable & ~Deinitable>(_ b: Bool, _ t: consuming T) -> T {
  if b {
    return t
  } else {
    return returnsGeneric(t)
  }
}

func consumesGenericOnOneBranch<T: ~Copyable & ~Deinitable>(
  _ b: Bool, _ t: consuming T, _ fallback: consuming T // expected-error {{'t' is not consumed on all paths}}
  // expected-error@-1 {{'fallback' is not consumed on all paths}}
) -> T {
  if b {
    return t // expected-note {{path exits here without a consume}}
  }
  return fallback // expected-note {{path exits here without a consume}}
}

func dropsGenericOnThrowingPath<T: ~Copyable & ~Deinitable>(_ t: consuming T) throws -> T { // expected-error {{'t' is not consumed on all paths}}
  try mayThrow() // expected-note {{path exits here without a consume}}
  return t
}

func wrapsGeneric<T: ~Copyable & ~Deinitable>(_ t: consuming T) -> Box<T> { Box(t: t) }

func unwrapsGeneric<T: ~Copyable & ~Deinitable>(_ b: consuming Box<T>) -> T { b.take() }

func dropsGenericContainer<T: ~Copyable & ~Deinitable>(_ b: consuming Box<T>) {} // expected-error {{'b' is not consumed on all paths}}
// expected-note@-1 {{path exits here without a consume}}

extension Box where T: ~Copyable & ~Deinitable {
  consuming func dropsSelf() {} // expected-error {{'self' is not consumed on all paths}}
  // expected-note@-1 {{path exits here without a consume}}
}

// FIXME: [deinitable] The synthesized setter of a stored property drops its old value. The error should name 'self.value'.
struct Wrapper<T: ~Copyable & ~Deinitable>: ~Copyable, ~Deinitable {
  var value: T
  // expected-error@-1 {{'self' is not consumed on all paths}}
  // expected-note@-2 {{path exits here without a consume}}
}

// FIXME: [deinitable] The error should name 'w.value'.
func replacesGenericStoredProperty<T: ~Copyable & ~Deinitable>(_ w: inout Wrapper<T>, _ t: consuming T) { // expected-error {{'w' is not consumed on all paths}}
  w.value = t // expected-note {{path exits here without a consume}}
}

// A generic value that's only noncopyable is still implicitly destroyable.
func dropsNoncopyableGeneric<T: ~Copyable>(_ t: consuming T) {}

struct NoncopyableBox<T: ~Copyable>: ~Copyable {
  var t: T
}

func dropsNoncopyableGenericContainer<T: ~Copyable>(_ b: consuming NoncopyableBox<T>) {}

// Copyable and ordinary noncopyable values can still be destroyed implicitly.
struct NC: ~Copyable {}

func ordinaryValues() {
  let _ = NC()
  let _ = [1, 2, 3]
}
