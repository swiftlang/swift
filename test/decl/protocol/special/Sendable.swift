// RUN: %target-typecheck-verify-swift -strict-concurrency=complete

func acceptSendable<T: Sendable>(_: T) { }

class NotSendable { } // expected-note 2{{class 'NotSendable' does not conform to the 'Sendable' protocol}}

@_nonSendable struct ExplicitlyNonSendable { // expected-note {{conformance of 'ExplicitlyNonSendable' to 'Sendable' has been explicitly marked unavailable here}}
  var int: Int = 0
}

@_nonSendable struct NonSendableWins: Sendable { }
@_nonSendable(_assumed) struct NonSendableLoses: Sendable { }

func testSendableBuiltinConformances(
  i: Int, ns: NotSendable, xns: ExplicitlyNonSendable,
  nsw: NonSendableWins, nsl: NonSendableLoses,
  sf: @escaping @Sendable () -> Void, nsf: @escaping () -> Void,
  mt: NotSendable.Type,
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
  acceptSendable(nsf) // expected-warning{{type '() -> Void' does not conform to the 'Sendable' protocol}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable((nsf, i)) // expected-warning{{type '() -> Void' does not conform to the 'Sendable' protocol}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable(funNotSendable) // expected-warning{{type '() -> Void' does not conform to the 'Sendable' protocol}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable((i, ns)) // expected-warning{{type 'NotSendable' does not conform to the 'Sendable' protocol}}

  // Explicitly @_nonSendable
  acceptSendable(xns) // expected-warning{{conformance of 'ExplicitlyNonSendable' to 'Sendable' is unavailable}}
  acceptSendable(nsw) // TODO: @_nonSendable should beat an explicit conformance, or at least conflict with it
  acceptSendable(nsl)
}
