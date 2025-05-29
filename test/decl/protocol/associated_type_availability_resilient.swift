// RUN: %swift -typecheck -verify -target %target-cpu-apple-macosx12 %s -enable-library-evolution
// REQUIRES: OS=macosx


protocol P { }
extension Int: P { }

@available(macOS 12, *)
protocol P1 {
  associatedtype A

  @available(macOS 13, *)
  associatedtype B: P
  // expected-error@-1{{associated type 'B' that is less available than its protocol must have a default}}

  @available(macOS 13, *)
  associatedtype C: P = Int
}
