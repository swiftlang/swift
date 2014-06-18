// RUN: %swift -parse -verify %s

protocol HasSelfRequirements {
  func foo(x: Self)

  // FIXME: Should raise an error here, but checking if the
  // existential conforms to itself while checking the protocol causes
  // infinite recursion.
  func returnsOwnProtocol() -> HasSelfRequirements
}
protocol Bar {
  // init() methods should not prevent use as an existential.
  init()

  func bar() -> Bar
}

func useBarAsType(x: Bar) {}

typealias Compo = protocol<HasSelfRequirements, Bar>

struct CompoAssocType {
  typealias Compo = protocol<HasSelfRequirements, Bar> // expected-error{{use of protocol 'HasSelfRequirements' as a type is not supported because it has associated type requirements}}
}

func useAsRequirement<T: HasSelfRequirements>(x: T) { }
func useCompoAsRequirement<T: protocol<HasSelfRequirements, Bar>>(x: T) { }
func useCompoAliasAsRequirement<T: Compo>(x: T) { }

func useAsWhereRequirement<T where T: HasSelfRequirements>(x: T) { }
func useCompoAsWhereRequirement<T where T: protocol<HasSelfRequirements, Bar>>(x: T) { }
func useCompoAliasAsWhereRequirement<T where T: Compo>(x: T) { }

func useAsType(x: HasSelfRequirements) { } // expected-error{{use of protocol 'HasSelfRequirements' as a type is not supported because it has associated type requirements}}
func useCompoAsType(x: protocol<HasSelfRequirements, Bar>) { } // expected-error{{use of protocol 'HasSelfRequirements' as a type is not supported because it has associated type requirements}}
func useCompoAliasAsType(x: Compo) { } // expected-error{{use of protocol 'HasSelfRequirements' as a type is not supported because it has associated type requirements}}

struct TypeRequirement<T: HasSelfRequirements> {}
struct CompoTypeRequirement<T: protocol<HasSelfRequirements, Bar>> {}
struct CompoAliasTypeRequirement<T: Compo> {}

struct CompoTypeWhereRequirement<T where T: protocol<HasSelfRequirements, Bar>> {}
struct CompoAliasTypeWhereRequirement<T where T: Compo> {}

