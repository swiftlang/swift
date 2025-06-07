// RUN: %target-typecheck-verify-swift

// This test focuses on the prevention of users from _writing_ types where
// a move-only type is substituted for a generic parameter.
//
// This means this is hitting on the TypeRepr -> Type phase in TypeCheckType.

// ----------------------
// ---   utilities    ---
// ----------------------

class Klass {}

let asdf: any Hashable & Klass

// a concrete move-only type
struct MO: ~Copyable {
  var x: Int?
}

struct GenericMO<T>: ~Copyable {
  var t: T
}

protocol Box<T> {
  associatedtype T
  func get() -> T
}

struct ValBox<T>: Box {
  var val: T
  init(_ t: T) { val = t }
  func get() -> T { return val }
}

enum Maybe<T> {
  case none
  case just(T)
}

struct CerebralValley<T> {
  struct GenericBro<U> {}
  struct TechBro {}
}

// ----------------------
// --- now some tests ---
// ----------------------

func basic_vararg(_ va: MO...) {} // expected-error {{noncopyable type 'MO' cannot be used within a variadic type yet}}

func illegalTypes<T>(_ t: T) {
  let _: Array<MO> // expected-error {{type 'MO' does not conform to protocol 'Copyable'}}
  let _: Maybe<MO> // expected-error {{type 'MO' does not conform to protocol 'Copyable'}}
  let _: Dictionary<MO, String> // expected-error {{type 'MO' does not conform to protocol 'Hashable'}}
  let _: [MO] // expected-error {{type 'MO' does not conform to protocol 'Copyable'}}
  let _: [String : MO] // expected-error {{type 'MO' does not conform to protocol 'Copyable'}}
  let _: [MO : MO] // expected-error {{type 'MO' does not conform to protocol 'Hashable'}}
  let _: [MO : T] // expected-error {{type 'MO' does not conform to protocol 'Hashable'}}

  _ = t as! ValBox<MO> // expected-error {{type 'MO' does not conform to protocol 'Copyable'}}

  let _: Optional<MO>
  let _: MO?
  let _: MO??
  let _: MO!
  let _: MO?!

  let _: Klass & MO // expected-error {{non-protocol, non-class type 'MO' cannot be used within a protocol-constrained type}}
  let _: any MO // expected-error {{'any' has no effect on concrete type 'MO'}}
  let _: any GenericMO<T> // expected-error {{'any' has no effect on concrete type 'GenericMO<T>'}}

  let _: CerebralValley<MO>.TechBro // expected-error {{type 'MO' does not conform to protocol 'Copyable'}}
  let _: CerebralValley<Int>.GenericBro<MO> // expected-error {{type 'MO' does not conform to protocol 'Copyable'}}

  let _: GenericMO<MO> // expected-error {{type 'MO' does not conform to protocol 'Copyable'}}
}

func illegalInExpr() {

}
