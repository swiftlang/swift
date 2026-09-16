// RUN: %target-swift-frontend -typecheck -verify -swift-version 6 -strict-memory-safety:migrate %s

// An always-unsafe use is an error even in migration mode: unlike a merely
// unsafe one, it is not something a migration pass gets to rewrite silently.
// This is deliberate, and means a migration build cannot complete over code
// that uses an '@unsafe(always)' declaration without acknowledging it.

@unsafe(always) func alwaysUnsafeFunc() { }

@unsafe func unsafeFunc() { }

func testAlwaysUnsafe() {
  alwaysUnsafeFunc()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{3-3=unsafe }}
  // expected-note@-2{{reference to unsafe global function 'alwaysUnsafeFunc()'}}

  unsafe alwaysUnsafeFunc()
}

// A merely unsafe use still gets the migratable warning with its fix-it.
func testUnsafe() {
  unsafeFunc()
  // expected-warning@-1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  // expected-note@-2{{reference to unsafe global function 'unsafeFunc()'}}
}
