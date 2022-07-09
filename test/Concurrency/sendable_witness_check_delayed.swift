// RUN: %target-typecheck-verify-swift

// This triggers a conformance check with SuppressDiagnostics=true.
let x = S().f {}

protocol P {
  associatedtype A

  func f(_: A) -> Int // expected-note {{expected sendability to match requirement here}}
}

struct S : P {
  typealias A = () -> ()
  func f(_: @Sendable () -> ()) -> Int { return 0 }
  // expected-warning@-1 {{sendability of function types in instance method 'f' does not match requirement in protocol 'P'}}
}
