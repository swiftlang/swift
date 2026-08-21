// RUN: %target-typecheck-verify-swift -strict-memory-safety

// How the two flavors of unsafety interact when strict memory safety checking
// is enabled: '@unsafe(always)' declarations are diagnosed as errors while
// merely unsafe ones remain warnings.

@unsafe func unsafeFunc() { }

func testUnsafe() {
  unsafeFunc()
  // expected-warning@-1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{documentation-file=strict-memory-safety}}
  // expected-note@-2{{reference to unsafe global function 'unsafeFunc()'}}

  unsafe unsafeFunc()
}

@unsafe(always) func alwaysUnsafeValue() -> Int { 0 }

@unsafe func unsafeValue() -> Int { 0 }

func takesTwo(_: Int, _: Int) { }

// A single expression that involves both kinds is diagnosed once, with the
// always-unsafe wording. Unlike the non-strict case, the merely unsafe use is
// still reported.
func testMixed() {
  takesTwo(alwaysUnsafeValue(), unsafeValue())
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to unsafe global function 'alwaysUnsafeValue()'}}
  // expected-note@-3{{reference to unsafe global function 'unsafeValue()'}}

  unsafe takesTwo(alwaysUnsafeValue(), unsafeValue())
}

// -----------------------------------------------------------------------
// Compiler-synthesized code
// -----------------------------------------------------------------------

// Synthesized code is never treated as always-unsafe, so here it falls back to
// the merely-unsafe warning rather than an error.
@unsafe(always)
struct AlwaysUnsafeCodable: Codable {
  var value: Int
}

@propertyWrapper
@unsafe(always)
struct AlwaysUnsafeWrapper {
  var wrappedValue: Int
  init(wrappedValue: Int) { unsafe self.wrappedValue = wrappedValue }
}

@safe
struct UsesWrapper {
  @AlwaysUnsafeWrapper var value: Int = 0
  // expected-warning@-1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{documentation-file=strict-memory-safety}}
  // expected-note@-2{{argument 'self' in call to initializer 'init' has unsafe type 'AlwaysUnsafeWrapper.Type'}}
  // expected-note@-3{{reference to initializer 'init(wrappedValue:)' involves unsafe type 'AlwaysUnsafeWrapper'}}
}
