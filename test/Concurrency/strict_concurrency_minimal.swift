// RUN: %target-typecheck-verify-swift -strict-concurrency=minimal
// REQUIRES: concurrency

class C1 { }
// expected-note@-1{{class 'C1' does not conform to the 'Sendable' protocol}}

@_nonSendable class C2 { }
// expected-note@-1 2{{class 'C2' does not conform to the 'Sendable' protocol}}

class C3 { }
// expected-note@-1 2{{class 'C3' does not conform to the 'Sendable' protocol}}

@available(*, unavailable)
extension C3: Sendable { }

struct S1: Sendable {
  let c1: C1 // expected-warning{{stored property 'c1' of 'Sendable'-conforming struct 'S1' has non-sendable type 'C1'}}
  let c2: C2 // expected-warning{{stored property 'c2' of 'Sendable'-conforming struct 'S1' has non-sendable type 'C2'}}
  let c3: C3 // expected-warning{{stored property 'c3'}}
}

struct S2 {
  let c1: C1
}

struct S3 {
  let c2: C2
}


func takeSendable(_ body: @Sendable () -> Void) {
}

@available(SwiftStdlib 5.1, *)
func passSendable(
    c1: C1, c2: C2, c3: C3, fn: @escaping () -> Void, s1: S1, s2: S2, s3: S3
) async {
  // Don't warn about implicitly non-Sendable types
  takeSendable { print(c1) }
  takeSendable { print(fn) }

  // Warn about explicitly non-Sendable types
  takeSendable { print(c2) } // expected-warning{{capture of 'c2' with non-sendable type 'C2' in a `@Sendable` closure}}
  takeSendable { print(c3) } // expected-warning{{capture of 'c3' with non-sendable type 'C3' in a `@Sendable` closure}}

  // Don't warn about explicitly Sendable type, even when it's wrong.
  takeSendable { print(s1) }

  // Don't warn when we wrapped an implicitly non-Sendable type in a struct.
  takeSendable { print(s2) }

  // FIXME: Ideally, we would warn about cases where a type in this module is
  // inferred to be non-Sendable based on something explicitly non-Sendable,
  // like in the case below.
  takeSendable { print(s3) }
}
