// Declaration-modifier forms that used to crash ASTGen.

// RUN: %target-typecheck-verify-swift \
// RUN:   -verify-additional-prefix astgen- \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift \
// RUN:   -verify-additional-prefix cxx-

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// 'yielding' is not a modifier of its own -- it is part of the spelling of the
// 'yielding borrow' accessor kind. ASTGen used to hand it to the declaration
// modifier path, which found no attribute for it and crashed. With the
// CoroutineAccessors feature off, the accessor itself is diagnosed, exactly
// once.
var yieldingBorrowWithFeatureOff: Int {
  yielding borrow {
    // expected-error@-1 {{'yielding borrow' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// An unrecognized argument to 'unowned' is accepted by the parser without
// complaint, so ASTGen has to reject it or this would be accepted where the C++
// parser rejects it.
class Referent {}
class Holder {
  unowned(bogus) var a: Referent?
  // expected-error@-1 {{unknown option 'bogus' for attribute 'unowned'}}
}

// These are all rejected by the parser itself, so ASTGen recovers silently
// rather than adding a second diagnostic. The wording differs between the two
// parsers, so each side asserts its own.
nonisolated(something) func badNonisolatedArgument() async {}
// expected-astgen-error@-1 {{expected identifier in modifier}}
// expected-astgen-error@-2 {{unexpected code 'something' in modifier}}
// expected-astgen-note@-3 {{insert identifier}}
// expected-cxx-error@-4 {{consecutive statements on a line must be separated by ';'}}
// expected-cxx-error@-5 {{cannot find 'nonisolated' in scope}}
// expected-cxx-error@-6 {{cannot find 'something' in scope}}

// The only argument 'public' admits is '(set)'.
public(get) var badAccessControlArgument = 1
// expected-astgen-error@-1 {{expected 'set' in modifier}}
// expected-astgen-error@-2 {{internal variable cannot have a public setter}}
// expected-astgen-note@-3 {{replace 'get' with 'set'}}
// expected-cxx-error@-4 {{expected 'set' as subject of 'public' modifier}}

// Well-formed forms still work.
nonisolated(unsafe) var globalCounter = 0
public private(set) var readOnlyOutside = 0
class WeakHolder {
  weak var b: Referent?
  unowned(unsafe) var c: Referent?
  unowned(safe) var d: Referent?
}
