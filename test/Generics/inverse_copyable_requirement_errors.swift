// RUN: %target-typecheck-verify-swift

// a concrete move-only type
struct MO: ~Copyable {
  var x: Int?
}

func totallyInvalid<T>(_ t: T) where MO: ~Copyable {}
// expected-error@-1{{type 'MO' in conformance requirement does not refer to a generic parameter or associated type}}

func packingHeat<each T>(_ t: repeat each T) {} // expected-note {{generic parameter 'each T' has an implicit Copyable requirement}}
func packIt() {
  packingHeat(MO())  // expected-error {{noncopyable type 'MO' cannot be substituted for copyable generic parameter 'each T' in 'packingHeat'}}
  packingHeat(10)
}

func packingUniqueHeat_1<each T: ~Copyable>(_ t: repeat each T) {}
// expected-error@-1{{cannot suppress '~Copyable' on type 'each T'}}
// expected-note@-2{{generic parameter 'each T' has an implicit Copyable requirement}}
// expected-error@-3{{'each T' required to be 'Copyable' but is marked with '~Copyable'}}

func packingUniqueHeat_2<each T>(_ t: repeat each T)
   where repeat each T: ~Copyable {}
// expected-error@-1{{cannot suppress '~Copyable' on type 'each T'}}
// expected-note@-3{{generic parameter 'each T' has an implicit Copyable requirement}}
// expected-error@-3{{'each T' required to be 'Copyable' but is marked with '~Copyable'}}

func packItUniquely() {
  packingUniqueHeat_1(MO())
  // expected-error@-1{{noncopyable type 'MO' cannot be substituted for copyable generic parameter 'each T' in 'packingUniqueHeat_1'}}

  packingUniqueHeat_2(MO())
  // expected-error@-1{{noncopyable type 'MO' cannot be substituted for copyable generic parameter 'each T' in 'packingUniqueHeat_2'}}

  packingUniqueHeat_1(10)
}
