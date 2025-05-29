// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -swift-version 6

func acceptSendable<T: Sendable>(_: T) { }

class NotSendable { } // expected-note 2{{class 'NotSendable' does not conform to the 'Sendable' protocol}}

func testSendableBuiltinConformances(
  i: Int, ns: NotSendable, sf: @escaping @Sendable () -> Void,
  nsf: @escaping () -> Void, mt: NotSendable.Type,
  cf: @escaping @convention(c) () -> Void,
  funSendable: [(Int, @Sendable () -> Void, NotSendable.Type)?],
  funNotSendable: [(Int, () -> Void, NotSendable.Type)?]
) {
  acceptSendable(())
  acceptSendable(mt)
  acceptSendable(sf)
  acceptSendable(cf)
  acceptSendable((i, sf, mt))
  acceptSendable((i, label: sf))
  acceptSendable(funSendable)

  // Complaints about missing Sendable conformances
  acceptSendable((i, ns)) // expected-error{{type 'NotSendable' does not conform to the 'Sendable' protocol}}
  acceptSendable(nsf) // expected-error{{type '() -> Void' does not conform to the 'Sendable' protocol}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable((nsf, i)) // expected-error{{type '() -> Void' does not conform to the 'Sendable' protocol}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable(funNotSendable) // expected-error{{type '() -> Void' does not conform to the 'Sendable' protocol}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable((i, ns)) // expected-error{{type 'NotSendable' does not conform to the 'Sendable' protocol}}
}
