// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -solver-enable-diagnose-valid-salvage

// REQUIRES: OS=macosx

// rdar://190654977

// Verify that unavailable overloads used in a multi-statement closure
// are diagnosed as unavailable.

@available(macOS 13.0, *) func f(_: Int) {}
@available(macOS 13.0, *) func f(_: Double) {}

func take(_ body: () -> Void) {}

func testMultiStatementClosure() { // expected-note 2 {{add '@available' attribute to enclosing global function}}
  take {
    f(1) // expected-error {{'f' is only available in macOS 13.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    f(2) // expected-error {{'f' is only available in macOS 13.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
  }
}

func testSingleExpressionClosure() { // expected-note {{add '@available' attribute to enclosing global function}}
  take { f(1) } // expected-error {{'f' is only available in macOS 13.0 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}

func testNoClosure() { // expected-note {{add '@available' attribute to enclosing global function}}
  f(1) // expected-error {{'f' is only available in macOS 13.0 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}
