// RUN: %target-typecheck-verify-swift -enable-experimental-feature NondeinitableTypes

// REQUIRES: swift_feature_NondeinitableTypes

// The NondeinitableTypes feature lets structs, enums, generic parameters, and
// associated types suppress `Deinitable`, so that the compiler's support for
// `~Deinitable` types can be tested.

struct ND: ~Copyable, ~Deinitable { // expected-note {{struct 'ND' has '~Deinitable' constraint preventing 'Deinitable' conformance}}
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
