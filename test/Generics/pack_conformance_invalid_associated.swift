// RUN: %target-typecheck-verify-swift

// Regression test for a crash in `PackConformance::getAssociatedConformance`
// when one of the pattern conformances is invalid (here, because `S: P` fails
// its conformance check after `S` becomes ambiguous). The recursive
// associated-conformance lookup returned an invalid conformance whose
// `getType()` is null, and the null leaked into `PackType::get`.
// See https://github.com/swiftlang/swift/issues/88434.

protocol P {
  associatedtype A: P // expected-note{{protocol requires nested type 'A'}}
}

struct S: P { // expected-error{{type 'S' does not conform to protocol 'P'}}
              // expected-note@-1{{add stubs for conformance}}
              // expected-note@-2{{found this candidate}}
              // expected-note@-3{{'S' previously declared here}}
  typealias A = S // expected-error{{'S' is ambiguous for type lookup in this context}}
}

func f<each T: P>(_: repeat each T) -> (repeat (each T).A.A.A.A) {}

f(S(), S(), S()) // expected-warning{{result of call to 'f' is unused}}

public struct S {} // expected-note{{found this candidate}}
                   // expected-error@-1{{invalid redeclaration of 'S'}}
