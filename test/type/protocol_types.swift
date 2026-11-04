// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-
// RUN: %if swift_future %{ %target-typecheck-verify-swift -swift-version future -verify-additional-prefix future- %}

protocol HasSelfRequirements {
  func foo(_ x: Self)

  func returnsOwnProtocol() -> HasSelfRequirements // expected-swift6-warning {{use of protocol 'HasSelfRequirements' as a type must be written 'any HasSelfRequirements'}}
  // expected-future-error@-1 {{use of protocol 'HasSelfRequirements' as a type must be written 'any HasSelfRequirements'}}
}
protocol Bar {
  // init() methods should not prevent use as an existential.
  init()

  func bar() -> Bar // expected-future-error {{use of protocol 'Bar' as a type must be written 'any Bar'}}
}

func useBarAsType(_ x: Bar) {} // expected-future-error {{use of protocol 'Bar' as a type must be written 'any Bar'}}

protocol Pub : Bar { }

func refinementErasure(_ p: Pub) { // expected-future-error {{use of protocol 'Pub' as a type must be written 'any Pub'}}
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

func useAsType(_: any HasSelfRequirements,
               _: any HasSelfRequirements & Bar,
               _: any Compo,
               _: any CompoAssocType.Compo) { }

struct TypeRequirement<T: HasSelfRequirements> {}
struct CompoTypeRequirement<T: HasSelfRequirements & Bar> {}
struct CompoAliasTypeRequirement<T: Compo> {}
struct NestedCompoAliasTypeRequirement<T: CompoAssocType.Compo> {}

struct CompoTypeWhereRequirement<T> where T: HasSelfRequirements & Bar {}
struct CompoAliasTypeWhereRequirement<T> where T: Compo {}
struct NestedCompoAliasTypeWhereRequirement<T> where T: CompoAssocType.Compo {}

struct Struct1<T> { }
struct Struct2<T : Pub & Bar> { }
struct Struct3<T : Pub & Bar & P3> { } // expected-error {{cannot find type 'P3' in scope}}
struct Struct4<T> where T : Pub & Bar {}

struct Struct5<T : protocol<Pub, Bar>> { } // expected-error {{'protocol<...>' composition syntax has been removed; join the type constraints using '&'}}
struct Struct6<T> where T : protocol<Pub, Bar> {} // expected-error {{'protocol<...>' composition syntax has been removed; join the type constraints using '&'}}

typealias T1 = Pub & Bar
typealias T2 = protocol<Pub , Bar> // expected-error {{'protocol<...>' composition syntax has been removed; join the type constraints using '&'}}

// rdar://problem/20593294
protocol HasAssoc {
  associatedtype Assoc
  func foo()
}

do {
  enum MyError : Error {
    case bad(Any) // expected-warning {{associated value 'bad' of 'Sendable'-conforming enum 'MyError' has non-Sendable type 'Any'}}
  }

  func checkIt(_ js: Any) throws {
    switch js {
    case let dbl as HasAssoc: // expected-swift6-warning {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
    // expected-future-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
      throw MyError.bad(dbl)

    default:
      fatalError("wrong")
    }
  }
}

func testHasAssoc(_ x: Any, _: HasAssoc) { // expected-swift6-warning {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
// expected-future-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
  if let p = x as? any HasAssoc {
    p.foo() // don't crash here.
  }

  struct ConformingType : HasAssoc {
    typealias Assoc = Int
    func foo() {}

    func method() -> HasAssoc {} // expected-swift6-warning {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
    // expected-future-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
  }
}

// https://github.com/apple/swift/issues/42661
var b: HasAssoc // expected-swift6-warning {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
// expected-future-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}

// Further generic constraint error testing - typealias used inside statements
protocol P {}
typealias MoreHasAssoc = HasAssoc & P
func testHasMoreAssoc(_ x: Any) {
  if let p = x as? any MoreHasAssoc {
    p.foo() // don't crash here.
  }
}

struct Outer {
  typealias Any = Int // expected-error {{keyword 'Any' cannot be used as an identifier here}} expected-note {{if this name is unavoidable, use backticks to escape it}} {{13-16=`Any`}}
  typealias `Any` = Int
  static func aa(a: `Any`) -> Int { return a }
}

typealias X = Struct1<Pub & Bar>
// expected-future-error@-1 {{use of protocol 'Pub' as a type must be written 'any Pub'}}
// expected-future-error@-2 {{use of protocol 'Bar' as a type must be written 'any Bar'}}
_ = Struct1<Pub & Bar>.self
// expected-future-error@-1 {{use of protocol 'Pub' as a type must be written 'any Pub'}}
// expected-future-error@-2 {{use of protocol 'Bar' as a type must be written 'any Bar'}}

typealias AliasWhere<T> = T
where T : HasAssoc, T.Assoc == HasAssoc // expected-swift6-warning {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
// expected-future-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}

struct StructWhere<T>
where T : HasAssoc,
      T.Assoc == any HasAssoc {}

protocol ProtocolWhere where T == HasAssoc { // expected-swift6-warning {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
// expected-future-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
  associatedtype T

  associatedtype U : HasAssoc
    where U.Assoc == any HasAssoc
}

extension HasAssoc where Assoc == HasAssoc {} // expected-swift6-warning {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
// expected-future-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}

func FunctionWhere<T>(_: T)
where T : HasAssoc,
      T.Assoc == any HasAssoc {}

struct SubscriptWhere {
  subscript<T>(_: T) -> Int
  where T : HasAssoc,
        T.Assoc == any HasAssoc {
    get {}
    set {}
  }
}

struct OuterGeneric<T> {
  func contextuallyGenericMethod() where T == any HasAssoc {}
}

typealias HasAssocAlias = HasAssoc

func testExistentialInCase(_ x: Any) {
  switch x {
  case is HasAssoc:
    // expected-swift6-warning@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
    // expected-future-error@-2 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
    break
  default:
    break
  }
  _ = {
    switch x {
    case is HasAssoc:
      // expected-swift6-warning@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
      // expected-future-error@-2 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
      break
    default:
      break
    }
  }
  switch x {
  case is HasAssocAlias:
    // expected-swift6-warning@-1 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
    // expected-future-error@-2 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
    break
  default:
    break
  }
  _ = {
    switch x {
    case is HasAssocAlias:
      // expected-swift6-warning@-1 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
      // expected-future-error@-2 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
      break
    default:
      break
    }
  }
  switch x {
  case is ~Copyable:
    // expected-swift6-warning@-1 {{constraint that suppresses conformance requires 'any'}}
    // expected-warning@-2 {{'is' test is always true}}
    // expected-future-error@-3 {{constraint that suppresses conformance requires 'any'}}
    break
  default:
    break
  }
  _ = {
    switch x {
    case is ~Copyable:
      // expected-swift6-warning@-1 {{constraint that suppresses conformance requires 'any'}}
      // expected-warning@-2 {{'is' test is always true}}
      // expected-future-error@-3 {{constraint that suppresses conformance requires 'any'}}
      break
    default:
      break
    }
  }
}

func throwingFn() throws {}

// These are downgraded to warnings until a future language mode, since they
// were previously missed.
// https://github.com/swiftlang/swift/issues/77553
func testExistentialInCatch() throws {
  do {
    try throwingFn()
  } catch is HasAssoc {}
  // expected-swift6-warning@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
  // expected-future-error@-2 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
  _ = {
    do {
      try throwingFn()
    } catch is HasAssoc {}
    // expected-swift6-warning@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
    // expected-future-error@-2 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
  }
  do {
    try throwingFn()
  } catch is HasAssocAlias {}
  // expected-swift6-warning@-1 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
  // expected-future-error@-2 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
  _ = {
    do {
      try throwingFn()
    } catch is HasAssocAlias {}
    // expected-swift6-warning@-1 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
    // expected-future-error@-2 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
  }
  do {
    try throwingFn()
  } catch is ~Copyable {}
  // expected-swift6-warning@-1 {{constraint that suppresses conformance requires 'any'}}
  // expected-warning@-2 {{'is' test is always true}}
  // expected-future-error@-3 {{constraint that suppresses conformance requires 'any'}}

  // FIXME: We shouldn't emit a duplicate 'always true' warning here.
  _ = {
    do {
      try throwingFn()
    } catch is ~Copyable {}
    // expected-swift6-warning@-1 {{constraint that suppresses conformance requires 'any'}}
    // expected-warning@-2 2{{'is' test is always true}}
    // expected-future-error@-3 {{constraint that suppresses conformance requires 'any'}}
  }
}
