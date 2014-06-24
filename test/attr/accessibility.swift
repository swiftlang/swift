// RUN: %swift %s -parse -verify

// CHECK PARSING
@private // expected-note {{attribute already specified here}}
@private // expected-error {{duplicate attribute}}
func duplicateAttr() {}

@private // expected-note {{attribute already specified here}}
@public // expected-error {{duplicate attribute}}
func duplicateAttrChanged() {}

@private // expected-note 2 {{attribute already specified here}}
@public // expected-error {{duplicate attribute}}
@internal // expected-error {{duplicate attribute}}
func triplicateAttrChanged() {}

@private(set)
@public
var customSetter = 0

@private(set) // expected-note {{attribute already specified here}}
@public(set) // expected-error {{duplicate attribute}}
var customSetterDuplicateAttr = 0

@private(set) // expected-note {{attribute already specified here}}
@public // expected-note {{attribute already specified here}}
@public(set) // expected-error {{duplicate attribute}}
@private // expected-error {{duplicate attribute}}
var customSetterDuplicateAttrsAllAround = 0

@private(get) // expected-error{{expected 'set' as subject of 'private' attribute}}
var invalidSubject = 0

@private(42) // expected-error{{expected 'set' as subject of 'private' attribute}}
var invalidSubject2 = 0

@private(a bunch of random tokens) // expected-error{{expected 'set' as subject of 'private' attribute}} expected-error{{expected declaration}}
var invalidSubject3 = 0

@private(set // expected-error{{expected ')' in 'private' attribute}}
var unterminatedSubject = 0

@private(42 // expected-error{{expected 'set' as subject of 'private' attribute}} expected-error{{expected declaration}}
var unterminatedInvalidSubject = 0

@private() // expected-error{{expected 'set' as subject of 'private' attribute}}
var emptySubject = 0

@private( // expected-error{{expected 'set' as subject of 'private' attribute}}
var unterminatedEmptySubject = 0

// Check that the parser made it here.
duplicateAttr(1) // expected-error{{}}


// CHECK ALLOWED DECLS
@private import Swift // expected-error {{'private' attribute cannot be applied to this declaration}}

@private typealias MyInt = Int

@private struct TestStruct {
  @private typealias LocalInt = MyInt
  @private var x = 0
  @private let y = 1
  @private func method() {}
  @private static func method() {}
  @private init() {}
  @private subscript(_: MyInt) -> LocalInt { return x }
}

@private class TestClass {
  @private init() {}
  @internal deinit {} // expected-error {{'internal' attribute cannot be applied to this declaration}}
}

@private enum TestEnum {
  @private case Foo, Bar // expected-error {{'private' attribute cannot be applied to this declaration}}
}

@private protocol TestProtocol {
  @private typealias Foo // expected-error {{'private' attribute cannot be used in protocols}}
  @internal var Bar: Int { get } // expected-error {{'internal' attribute cannot be used in protocols}}
  @public func baz() // expected-error {{'public' attribute cannot be used in protocols}}
}
