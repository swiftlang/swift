// RUN: %target-typecheck-verify-swift -warn-concurrency

func acceptSendable<T: Sendable>(_: T) { }

class NotSendable { }

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
  acceptSendable((i, ns)) // expected-warning{{type 'NotSendable' does not conform to the 'Sendable' protocol}}
  acceptSendable(nsf) // expected-warning{{function type '() -> Void' must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable((nsf, i)) // expected-warning{{function type '() -> Void' must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable(funNotSendable) // expected-warning{{function type '() -> Void' must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable((i, ns)) // expected-warning{{type 'NotSendable' does not conform to the 'Sendable' protocol}}
}
