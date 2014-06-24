// RUN: %swift %s -parse -verify

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
