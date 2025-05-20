// RUN: %target-swift-frontend -typecheck -verify -swift-version 6 -strict-memory-safety:migrate %s

// REQUIRES: concurrency

@preconcurrency import _Concurrency

@unsafe func f() { }

func g() {
  f() // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  // expected-note@-1{{reference to unsafe global function 'f()'}}
}

protocol P {
  func f()
}

struct Conforming: P {
  // expected-warning@-1{{conformance of 'Conforming' to protocol 'P' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe}}{{20-20=@unsafe }}
  @unsafe func f() { } // expected-note{{unsafe instance method 'f()' cannot satisfy safe requirement}}
}
