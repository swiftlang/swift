// RUN: %target-typecheck-verify-swift -enable-experimental-feature NondeinitableTypes -verify-additional-prefix copytuples- -verify-ignore-unrelated
// RUN: %target-typecheck-verify-swift -enable-experimental-feature NondeinitableTypes -verify-additional-prefix copytuples- -verify-ignore-unrelated -swift-version 5
// RUN: %target-typecheck-verify-swift -enable-experimental-feature NondeinitableTypes -enable-experimental-feature MoveOnlyTuples -verify-additional-prefix movetuples- -verify-ignore-unrelated

// REQUIRES: swift_feature_NondeinitableTypes
// REQUIRES: swift_feature_MoveOnlyTuples

// The NondeinitableTypes feature lets structs, enums, generic parameters, and
// associated types suppress `Deinitable`, so that the compiler's support for
// `~Deinitable` types can be tested.

struct ND: ~Copyable, ~Deinitable { // expected-note 3 {{struct 'ND' has '~Deinitable' constraint preventing 'Deinitable' conformance}}
  consuming func finish() {
    discard self // Ok, even without a deinit
  }
}

enum E: ~Copyable, ~Deinitable {
  case a
}

func suppressing<T: ~Copyable & ~Deinitable>(_: consuming T) {}
func whereClause<T>(_: consuming T) where T: ~Copyable, T: ~Deinitable {}

protocol HasND { associatedtype A: ~Copyable, ~Deinitable }
protocol HasNDWhereClause where A: ~Copyable, A: ~Deinitable {
  associatedtype A
}
struct ConformsWithND: HasND { typealias A = ND }
func associatedType<T: HasND>(_: T.Type, _: consuming T.A) {}

// `Copyable` implies `Deinitable`.
struct CopyableButNot: ~Deinitable {}
// expected-error@-1 {{struct 'CopyableButNot' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}

func copyableButNot<T: ~Deinitable>(_: T) {}
// expected-error@-1 {{'T' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}

protocol CopyableButNotAssoc { associatedtype A: ~Deinitable }
// expected-error@-1 {{'Self.A' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}

func explicitlyCopyableButNot<T: Copyable & ~Deinitable>(_: T) {}
// expected-error@-1 {{'T' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}

func escapableButNot<T: ~Escapable & ~Deinitable>(_: borrowing T) {}
// expected-error@-1 {{'T' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}

func someCopyableButNot(_: borrowing some ~Deinitable) {}
// expected-error@-1 {{'some ~Deinitable' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}

func someNondeinitable(_: consuming some ~Copyable & ~Deinitable) {} // Ok

struct NoncopyableHolder<T: ~Copyable>: ~Copyable {}
extension NoncopyableHolder where T: ~Deinitable {}
// expected-error@-1 {{'T' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}

// Each subject must suppress both, even if a same-type requirement makes it
// equivalent to a subject that suppresses `Copyable`.
func sameType<T: ~Copyable & ~Deinitable, U: ~Deinitable>(_: borrowing T, _: borrowing U) where T == U {}
// expected-error@-1 {{'U' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}
// expected-warning@-2 {{same-type requirement makes generic parameters 'U' and 'T' equivalent}}

func sameTypeWhereClause<T, U>(_: borrowing T, _: borrowing U) where T: ~Copyable, U: ~Deinitable, T == U {}
// expected-error@-1 {{'U' cannot suppress 'Deinitable' without also suppressing 'Copyable'}}
// expected-warning@-2 {{same-type requirement makes generic parameters 'U' and 'T' equivalent}}

func sameTypeBoth<T, U>(_: borrowing T, _: borrowing U) where T: ~Copyable, T: ~Deinitable, U: ~Copyable, U: ~Deinitable, T == U {} // Ok
// expected-warning@-1 {{same-type requirement makes generic parameters 'U' and 'T' equivalent}}

// A `~Deinitable` type can't become `Copyable`, even conditionally.
struct ConditionallyCopyable<T: ~Copyable & ~Deinitable>: ~Copyable, ~Deinitable {}
extension ConditionallyCopyable: Copyable where T: Copyable {}
// expected-error@-1 {{type 'ConditionallyCopyable<T>' does not conform to protocol 'Deinitable'}}

// A `~Copyable` protocol still requires `Deinitable`.
protocol NoncopyableProto: ~Copyable {}
// expected-note@-1 {{type 'NondeinitableConformer' does not conform to inherited protocol 'Deinitable'}}
struct NondeinitableConformer: ~Copyable, ~Deinitable, NoncopyableProto {}
// expected-error@-1 {{type 'NondeinitableConformer' does not conform to protocol 'Deinitable'}}

// A `~Deinitable` type can't have an implicit destructor.
struct WithDeinit: ~Copyable, ~Deinitable {
  deinit {} // expected-error {{deinitializer cannot be declared in struct 'WithDeinit', which suppresses 'Deinitable'}}
}

// Other declarations can't suppress `Deinitable`, even with the feature.
protocol P: ~Copyable, ~Deinitable {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
// expected-warning@-1 {{protocol 'P' should be declared to refine 'Deinitable' due to a same-type constraint on 'Self'}}
func existential(_: any ~Copyable & ~Deinitable) {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
class C: ~Deinitable {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
// expected-error@-1 {{classes cannot be '~Deinitable'}}
func requiring<T: Deinitable>(_: T) {} // expected-error {{'Deinitable' is reserved for use by the compiler}}

// Containment
struct Holder: ~Copyable {
  var nd: ND // expected-error {{stored property 'nd' of 'Deinitable'-conforming struct 'Holder' has non-Deinitable type 'ND'}}
}

struct DeinitableHolder: ~Copyable, ~Deinitable {
  var nd: ND // Ok
}

// Generic requirements
func noncopyable<T: ~Copyable>(_: consuming T) {} // expected-note 2 {{'where T: Deinitable' is implicit here}}

func useND() {
  suppressing(ND()) // Ok
  whereClause(ND()) // Ok
  noncopyable(ND()) // expected-error {{global function 'noncopyable' requires that 'ND' conform to 'Deinitable'}}
}

func useAssociatedType<T: HasND>(_: T.Type, _ a: consuming T.A) {
  noncopyable(a) // expected-error {{global function 'noncopyable' requires that 'T.A' conform to 'Deinitable'}}
}

struct Box<T: ~Copyable>: ~Copyable {
  var value: T
}

func spelled(_: consuming Box<ND>) {} // expected-error {{type 'ND' does not conform to protocol 'Deinitable'}}

// MARK: - Storage that nothing consumes explicitly

func makeND() -> ND { ND() }

class ClassHolder {
  var nd: ND // expected-error {{stored property 'nd' of 'Deinitable'-conforming class 'ClassHolder' has non-Deinitable type 'ND'}}
  init() { nd = ND() }
}

actor ActorHolder {
  let nd = ND() // expected-error {{stored property 'nd' of 'Deinitable'-conforming actor 'ActorHolder' has non-Deinitable type 'ND'}}
}

let globalND = ND() // expected-error {{global variable 'globalND' cannot have non-Deinitable type 'ND'}}

struct StaticHolder: ~Copyable, ~Deinitable {
  static let staticND = ND() // expected-error {{static property 'staticND' cannot have non-Deinitable type 'ND'}}
}

class LazyHolder {
  lazy var nd = ND() // expected-error {{lazy property 'nd' cannot have non-Deinitable type 'ND'}}
}

func asyncLet() async {
  async let x = makeND() // expected-error {{'async let' binding 'x' cannot have non-Deinitable type 'ND'}}
  _ = await x
}

func tuples<T: ~Copyable & ~Deinitable>(_ t: consuming T) {
  let _: (ND, Int) = (ND(), 0)
  // expected-movetuples-error@-1 2 {{tuple cannot contain non-Deinitable element type 'ND'}}
  // expected-copytuples-error@-2 2 {{tuple with noncopyable element type 'ND' is not supported}}
  let _ = (makeND(), 1)
  // expected-movetuples-error@-1 {{tuple cannot contain non-Deinitable element type 'ND'}}
  // expected-copytuples-error@-2 {{tuple with noncopyable element type 'ND' is not supported}}
  let _ = (t, 1)
  // expected-movetuples-error@-1 {{tuple cannot contain non-Deinitable element type 'T'}}
  // expected-copytuples-error@-2 {{tuple with noncopyable element type 'T' is not supported}}
}

// MARK: - Optional and existentials

func optionalAndExistential() {
  // FIXME: [deinitable] Optional's payload must be Deinitable.
  let _: _? = ND()
  let _: Optional<ND> = nil // expected-error {{type 'ND' does not conform to protocol 'Deinitable'}}
  let _: any ~Copyable = ND() // expected-error {{value of type 'ND' does not conform to specified type 'Deinitable'}}
}

struct MakesND {
  func make() -> ND { ND() }
  func makeOrThrow() throws -> ND { ND() }
}

func inferredOptional(_ m: MakesND?, _ n: MakesND) {
  // FIXME: [deinitable] Optional's payload must be Deinitable.
  _ = m?.make()
  // FIXME: [deinitable] Optional's payload must be Deinitable.
  _ = try? n.makeOrThrow()
}

// Local variables keep the obligation with their scope.
func locals() {
  let nd = ND()
  nd.finish()
}

// MARK: - Sendable

// `Sendable` and `SendableMetatype` don't require `Deinitable`.
struct ExplicitlySendable: ~Copyable, ~Deinitable, Sendable {}

func requireSendable<T: ~Copyable & ~Deinitable & Sendable>(_ t: consuming T) -> T { t }
func requireSendableMetatype<T: ~Copyable & ~Deinitable & SendableMetatype>(_: T.Type) {}

func sendable(_ s: consuming ExplicitlySendable) -> ExplicitlySendable {
  requireSendableMetatype(ND.self)
  return requireSendable(s)
}

// A `~Deinitable` type is implicitly `Sendable` like any other.
func implicitlySendable(_ nd: consuming ND) -> ND {
  requireSendable(nd)
}
