// RUN: %target-parse-verify-swift

protocol HasSelfRequirements {
  func foo(_ x: Self)

  func returnsOwnProtocol() -> HasSelfRequirements // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint because it has Self or associated type requirements}}
}
protocol Bar {
  // init() methods should not prevent use as an existential.
  init()

  func bar() -> Bar
}

func useBarAsType(_ x: Bar) {}

protocol Pub : Bar { }

func refinementErasure(_ p: Pub) {
  useBarAsType(p)
}

typealias Compo = protocol<HasSelfRequirements, Bar>

struct CompoAssocType {
  typealias Compo = protocol<HasSelfRequirements, Bar> // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint}}
}

func useAsRequirement<T: HasSelfRequirements>(_ x: T) { }
func useCompoAsRequirement<T: protocol<HasSelfRequirements, Bar>>(_ x: T) { }
func useCompoAliasAsRequirement<T: Compo>(_ x: T) { }

func useAsWhereRequirement<T where T: HasSelfRequirements>(_ x: T) { }
func useCompoAsWhereRequirement<T where T: protocol<HasSelfRequirements, Bar>>(_ x: T) { }
func useCompoAliasAsWhereRequirement<T where T: Compo>(_ x: T) { }

func useAsType(_ x: HasSelfRequirements) { } // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint}}
func useCompoAsType(_ x: protocol<HasSelfRequirements, Bar>) { } // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint}}
func useCompoAliasAsType(_ x: Compo) { } // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint}}

struct TypeRequirement<T: HasSelfRequirements> {}
struct CompoTypeRequirement<T: protocol<HasSelfRequirements, Bar>> {}
struct CompoAliasTypeRequirement<T: Compo> {}

struct CompoTypeWhereRequirement<T where T: protocol<HasSelfRequirements, Bar>> {}
struct CompoAliasTypeWhereRequirement<T where T: Compo> {}


// rdar://problem/20593294
protocol HasAssoc {
  associatedtype Assoc
  func foo()
}

func testHasAssoc(_ x: Any) {
  if let p = x as? HasAssoc { // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint}}
    p.foo() // don't crash here.
  }
}

// rdar://problem/16803384
protocol InheritsAssoc : HasAssoc {
  func silverSpoon()
}

func testInheritsAssoc(_ x: InheritsAssoc) { // expected-error {{protocol 'InheritsAssoc' can only be used as a generic constraint}}
  x.silverSpoon()
}

// SR-38
var b: HasAssoc // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint because it has Self or associated type requirements}}

// Further generic constraint error testing - typealias used inside statements
protocol P {}
typealias MoreHasAssoc = protocol<HasAssoc, P>
func testHasMoreAssoc(_ x: Any) {
  if let p = x as? MoreHasAssoc { // expected-error {{protocol 'HasAssoc' can only be used as a generic constraint}}
    p.foo() // don't crash here.
  }
}


