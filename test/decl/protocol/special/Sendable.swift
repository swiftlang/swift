// RUN: %target-typecheck-verify-swift

func acceptSendable<T: Sendable>(_: T) { } // expected-note{{required by global function 'acceptSendable' where 'T' = '() -> Void'}}

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

  // Errors
  acceptSendable((i, ns)) // expected-error{{global function 'acceptSendable' requires that 'NotSendable' conform to 'Sendable'}}
  acceptSendable(nsf) // expected-error{{type '() -> Void' cannot conform to 'Sendable'}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  acceptSendable((nsf, i)) // expected-error{{type '() -> Void' cannot conform to 'Sendable'}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  // expected-note@-2{{requirement from conditional conformance of '(() -> Void, Int)' to 'Sendable'}}
  acceptSendable(funNotSendable) // expected-error{{type '() -> Void' cannot conform to 'Sendable'}}
  // expected-note@-1{{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  // expected-note@-2{{requirement from conditional conformance of '(Int, () -> Void, NotSendable.Type)' to 'Sendable'}}
  acceptSendable((i, ns)) // expected-error{{global function 'acceptSendable' requires that 'NotSendable' conform to 'Sendable'}}
}
