// RUN: %target-typecheck-verify-swift -swift-version 7
// REQUIRES: swift7

protocol HasAssoc {
  associatedtype Assoc
}

typealias HasAssocAlias = HasAssoc

func throwingFn() throws {}

// In Swift 6 we previously missed this diagnostic.
// https://github.com/swiftlang/swift/issues/77553
func testExistentialInCatch() throws {
  do {
    try throwingFn()
  } catch is HasAssoc {}
  // expected-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
  _ = {
    do {
      try throwingFn()
    } catch is HasAssoc {}
    // expected-error@-1 {{use of protocol 'HasAssoc' as a type must be written 'any HasAssoc'}}
  }
  do {
    try throwingFn()
  } catch is HasAssocAlias {}
  // expected-error@-1 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
  _ = {
    do {
      try throwingFn()
    } catch is HasAssocAlias {}
    // expected-error@-1 {{use of 'HasAssocAlias' (aka 'HasAssoc') as a type must be written 'any HasAssocAlias' (aka 'any HasAssoc')}}
  }
  do {
    try throwingFn()
  } catch is ~Copyable {}
  // expected-error@-1 {{constraint that suppresses conformance requires 'any'}}
  // expected-warning@-2 {{'is' test is always true}}

  // FIXME: We shouldn't emit a duplicate 'always true' warning here.
  _ = {
    do {
      try throwingFn()
    } catch is ~Copyable {}
    // expected-error@-1 {{constraint that suppresses conformance requires 'any'}}
    // expected-warning@-2 2{{'is' test is always true}}
  }
}
