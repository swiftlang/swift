// RUN: %target-typecheck-verify-swift -enable-explicit-existential-types

protocol HasSelfRequirements {
  func foo(_ x: Self)

  func returnsOwnProtocol() -> HasSelfRequirements // expected-error {{protocol 'HasSelfRequirements' can only be used as a generic constraint because it has Self or associated type requirements}}
}
protocol Bar {
  init()

  func bar() -> any Bar
}

func useBarAsType(_ x: any Bar) {}

protocol Pub : Bar { }

func refinementErasure(_ p: any Pub) {
  useBarAsType(p)
}

typealias Compo = HasSelfRequirements & Bar

struct CompoAssocType {
  typealias Compo = HasSelfRequirements & Bar
}

func useAsRequirement<T: HasSelfRequirements>(_ x: T) { }
func useCompoAsRequirement<T: HasSelfRequirements & Bar>(_ x: T) { }
func useCompoAliasAsRequirement<T: Compo>(_ x: T) { }
func useNestedCompoAliasAsRequirement<T: CompoAssocType.Compo>(_ x: T) { }

func useAsWhereRequirement<T>(_ x: T) where T: HasSelfRequirements {}
func useCompoAsWhereRequirement<T>(_ x: T) where T: HasSelfRequirements & Bar {}
func useCompoAliasAsWhereRequirement<T>(_ x: T) where T: Compo {}
func useNestedCompoAliasAsWhereRequirement<T>(_ x: T) where T: CompoAssocType.Compo {}

func useAsType(_: any HasSelfRequirements, // expected-error {{protocol 'HasSelfRequirements' can only be used as a generic constraint because it has Self or associated type requirements}}
               _: any HasSelfRequirements & Bar, // expected-error {{protocol 'HasSelfRequirements' can only be used as a generic constraint because it has Self or associated type requirements}}
               _: any Compo, // expected-error {{protocol 'HasSelfRequirements' can only be used as a generic constraint because it has Self or associated type requirements}}
               _: any CompoAssocType.Compo) { } // expected-error {{protocol 'HasSelfRequirements' can only be used as a generic constraint because it has Self or associated type requirements}}

struct TypeRequirement<T: HasSelfRequirements> {}
struct CompoTypeRequirement<T: HasSelfRequirements & Bar> {}
struct CompoAliasTypeRequirement<T: Compo> {}
struct NestedCompoAliasTypeRequirement<T: CompoAssocType.Compo> {}

struct CompoTypeWhereRequirement<T> where T: HasSelfRequirements & Bar {}
struct CompoAliasTypeWhereRequirement<T> where T: Compo {}
struct NestedCompoAliasTypeWhereRequirement<T> where T: CompoAssocType.Compo {}

struct Struct1<T> { }

typealias T1 = Pub & Bar
typealias T2 = any Pub & Bar

protocol HasAssoc {
  associatedtype Assoc
  func foo()
}

do {
  enum MyError: Error {
    case bad(Any)
  }

  func checkIt(_ js: Any) throws {
    switch js {
    case let dbl as any HasAssoc: // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
      throw MyError.bad(dbl)

    default:
      fatalError("wrong")
    }
  }
}

func testHasAssoc(_ x: Any, _: any HasAssoc) { // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
  if let p = x as? any HasAssoc { // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
    p.foo()
  }

  struct ConformingType : HasAssoc {
    typealias Assoc = Int
    func foo() {}

    func method() -> any HasAssoc {} // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
  }
}

var b: any HasAssoc // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}

protocol P {}
typealias MoreHasAssoc = HasAssoc & P
func testHasMoreAssoc(_ x: Any) {
  if let p = x as? any MoreHasAssoc { // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
    p.foo()
  }
}

typealias X = Struct1<any Pub & Bar>
_ = Struct1<any Pub & Bar>.self

typealias AliasWhere<T> = T
where T: HasAssoc, T.Assoc == any HasAssoc // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}

struct StructWhere<T>
where T: HasAssoc,
      T.Assoc == any HasAssoc {} // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}

protocol ProtocolWhere where T == any HasAssoc { // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
  associatedtype T

  associatedtype U: HasAssoc
    where U.Assoc == any HasAssoc // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
}

extension HasAssoc where Assoc == any HasAssoc {} // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}

func FunctionWhere<T>(_: T)
where T : HasAssoc,
      T.Assoc == HasAssoc {} // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}

struct SubscriptWhere {
  subscript<T>(_: T) -> Int
  where T : HasAssoc,
        T.Assoc == HasAssoc { // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
    get {}
    set {}
  }
}

struct OuterGeneric<T> {
  func contextuallyGenericMethod() where T == HasAssoc {} // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}
}

func testInvalidAny() {
  struct S: HasAssoc {
    typealias Assoc = Int
    func foo() {}
  }
  let _: any S = S() // expected-error{{'any' has no effect on concrete type 'S'}}

  func generic<T: HasAssoc>(t: T) {
    let _: any T = t // expected-error{{'any' has no effect on type parameter 'T'}}
    let _: any T.Assoc // expected-error {{'any' has no effect on type parameter 'T.Assoc'}}
  }

  let _: any ((S) -> Void) = generic // expected-error{{'any' has no effect on concrete type '(S) -> Void'}}
}

func anyAny() {
  let _: any Any
  let _: any AnyObject
}

protocol P1 {}
protocol P2 {}
struct ConcreteComposition: P1, P2 {}

func testMetatypes() {
  let _: any P1.Type = ConcreteComposition.self
  let _: any (P1 & P2).Type = ConcreteComposition.self
}

func generic<T: any P1>(_ t: T) {} // expected-error {{type 'T' constrained to non-protocol, non-class type 'any P1'}}

protocol RawRepresentable {
  associatedtype RawValue
  var rawValue: RawValue { get }
}

enum E1: RawRepresentable {
  typealias RawValue = P1

  var rawValue: P1 {
    return ConcreteComposition()
  }
}

enum E2: RawRepresentable {
  typealias RawValue = any P1

  var rawValue: any P1 {
    return ConcreteComposition()
  }
}

public protocol MyError {}

extension MyError {
  static func ~=(lhs: any Error, rhs: Self) -> Bool {
    return true
  }
}

struct Wrapper {
  typealias E = Error
}

func typealiasMemberReferences(metatype: Wrapper.Type) {
  let _: Wrapper.E.Protocol = metatype.E.self
  let _: (any Wrapper.E).Type = metatype.E.self
}

func testAnyTypeExpr() {
  let _: (any P).Type = (any P).self

  func test(_: (any P).Type) {}
  test((any P).self)

  // expected-error@+2 {{expected member name or constructor call after type name}}
  // expected-note@+1 {{use '.self' to reference the type object}}
  let invalid = any P
  test(invalid)

  // Make sure 'any' followed by an identifier
  // on the next line isn't parsed as a type.
  func doSomething() {}

  let any = 10
  let _ = any
  doSomething()
}
