// RUN: %target-parse-verify-swift

protocol HasSelfRequirements {
  func foo(x: Self)

  func returnsOwnProtocol() -> HasSelfRequirements // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint because it has Self or associated type requirements}}
}
protocol Bar {
  // init() methods should not prevent use as an existential.
  init()

  func bar() -> Bar
}

func useBarAsType(x: Bar) {}

typealias Compo = protocol<HasSelfRequirements, Bar>

struct CompoAssocType {
  typealias Compo = protocol<HasSelfRequirements, Bar> // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint}}
}

func useAsRequirement<T: HasSelfRequirements>(x: T) { }
func useCompoAsRequirement<T: protocol<HasSelfRequirements, Bar>>(x: T) { }
func useCompoAliasAsRequirement<T: Compo>(x: T) { }

func useAsWhereRequirement<T where T: HasSelfRequirements>(x: T) { }
func useCompoAsWhereRequirement<T where T: protocol<HasSelfRequirements, Bar>>(x: T) { }
func useCompoAliasAsWhereRequirement<T where T: Compo>(x: T) { }

func useAsType(x: HasSelfRequirements) { } // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint}}
func useCompoAsType(x: protocol<HasSelfRequirements, Bar>) { } // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint}}
func useCompoAliasAsType(x: Compo) { } // expected-error{{protocol 'HasSelfRequirements' can only be used as a generic constraint}}

struct TypeRequirement<T: HasSelfRequirements> {}
struct CompoTypeRequirement<T: protocol<HasSelfRequirements, Bar>> {}
struct CompoAliasTypeRequirement<T: Compo> {}

struct CompoTypeWhereRequirement<T where T: protocol<HasSelfRequirements, Bar>> {}
struct CompoAliasTypeWhereRequirement<T where T: Compo> {}


// rdar://problem/20593294
protocol HasAssoc {
  typealias Assoc
  func foo()
}

func testHasAssoc(x: Any) {
  if let p = x as? HasAssoc { // expected-error 2{{protocol 'HasAssoc' can only be used as a generic constraint}}
    p.foo() // don't crash here.
  }
}
