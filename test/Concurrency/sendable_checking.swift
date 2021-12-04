// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency
// REQUIRES: OS=macosx

@available(SwiftStdlib 5.1, *)
struct NS1 { }

@available(SwiftStdlib 5.1, *)
@available(*, unavailable)
extension NS1: Sendable { }
// expected-note@-1 2{{conformance of 'NS1' to 'Sendable' has been explicitly marked unavailable here}}

@available(SwiftStdlib 5.1, *)
struct NS2 { // expected-note{{consider making struct 'NS2' conform to the 'Sendable' protocol}}
  var ns1: NS1
}

@available(SwiftStdlib 5.1, *)
struct NS3 { }

@available(SwiftStdlib 5.3, *)
extension NS3: Sendable { }

@available(SwiftStdlib 5.1, *)
func acceptCV<T: Sendable>(_: T) { }

func acceptSendableFn(_: @Sendable @escaping () -> Void) { }

@available(SwiftStdlib 5.1, *)
func testCV(
  ns1: NS1, ns1array: [NS1], ns2: NS2, ns3: NS3, fn: @escaping () -> Void
  // expected-note@-1{{parameter 'fn' is implicitly non-sendable}}
) {
  acceptCV(ns1)
  acceptCV(ns1array)
  acceptCV(ns2)
  acceptCV(ns3)
  acceptCV(fn)
  acceptSendableFn(fn) // expected-warning{{passing non-sendable parameter 'fn' to function expecting a @Sendable closure}}
}

@available(SwiftStdlib 5.1, *)
func testCV(
  ns1: NS1, ns1array: [NS1], ns2: NS2, ns3: NS3, fn: @escaping () -> Void
  // expected-note@-1{{parameter 'fn' is implicitly non-sendable}}
) async {
  acceptCV(ns1) // expected-warning{{conformance of 'NS1' to 'Sendable' is unavailable}}
  acceptCV(ns1array) // expected-warning{{conformance of 'NS1' to 'Sendable' is unavailable}}
  acceptCV(ns2) // expected-warning{{type 'NS2' does not conform to the 'Sendable' protocol}}
  acceptCV(ns3) // expected-warning{{conformance of 'NS3' to 'Sendable' is only available in macOS 11.0 or newer}}
  // expected-note@-1{{add 'if #available' version check}}
  acceptCV(fn) // expected-warning{{type '() -> Void' does not conform to the 'Sendable' protocol}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendableFn(fn) // expected-error{{passing non-sendable parameter 'fn' to function expecting a @Sendable closure}}
}

// rdar://83942484 - spurious Sendable diagnostics
@available(SwiftStdlib 5.1, *)
public protocol MyProto {
    func foo<F>(aFoo: F) async where F: Sendable
}

@available(SwiftStdlib 5.1, *)
public actor MyActor: MyProto {
  public func foo<F>(aFoo: F) async where F: Sendable { }
  public func bar<B>(aBar: B) async where B: Sendable { }
}
