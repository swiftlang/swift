// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-experimental-feature NontransferableTypes -verify-ignore-unrelated

// REQUIRES: swift_feature_NontransferableTypes

// Sendable implies Transferable, so ~Transferable types don't get an implicit
// Sendable conformance.

func takesSendable<T: Sendable>(_: T.Type) {}

struct NT: ~Transferable {}
// expected-note@-1 4 {{struct 'NT' explicitly suppresses conformance to 'Sendable' protocol}}
// expected-note@-2 2 {{struct 'NT' has '~Transferable' constraint preventing 'Transferable' conformance}}

enum NTEnum: ~Transferable {
  // expected-note@-1 {{enum 'NTEnum' explicitly suppresses conformance to 'Sendable' protocol}}
  case a
}

final class NTClass: ~Transferable {}
// expected-note@-1 2 {{class 'NTClass' explicitly suppresses conformance to 'Sendable' protocol}}

struct ExplicitNT: ~Transferable {
  // expected-note@-1 {{struct 'ExplicitNT' explicitly suppresses conformance to 'Sendable' protocol}}
  var nt: NT
}

struct ImplicitNT {
  // expected-note@-1 {{consider adding '~Transferable' to struct 'ImplicitNT'}}
  // expected-note@-2 {{consider making struct 'ImplicitNT' conform to the 'Sendable' protocol}}
  let nt: NT
  // expected-error@-1 {{stored property 'nt' of 'Transferable'-conforming struct 'ImplicitNT' has non-'Transferable' type 'NT'}}
}

struct ExplcitSendableWithNT: Sendable {
  // expected-note@-1 {{consider adding '~Transferable' to struct 'ExplcitSendableWithNT'}}
  let v: Int // Sendable
  let nt: NT
  // expected-error@-1 {{stored property 'nt' of 'Transferable'-conforming struct 'ExplcitSendableWithNT' has non-'Transferable' type 'NT'}}
  // expected-error@-2 {{stored property 'nt' of 'Sendable'-conforming struct 'ExplcitSendableWithNT' has non-Sendable type 'NT'}}
}

// Global actor isolation doesn't make a ~Transferable type Sendable either.
@MainActor struct MainActorNT: ~Transferable {}
// expected-note@-1 {{struct 'MainActorNT' explicitly suppresses conformance to 'Sendable' protocol}}

@MainActor final class MainActorNTClass: ~Transferable {}
// expected-note@-1 {{class 'MainActorNTClass' explicitly suppresses conformance to 'Sendable' protocol}}

// A subclass of a ~Transferable class isn't Transferable, so it isn't Sendable
// even with global actor isolation.
class NTRoot: ~Transferable {}
@MainActor class MainActorSubOfNTRoot: NTRoot {}
// expected-note@-1 {{class 'MainActorSubOfNTRoot' does not conform to the 'Sendable' protocol}}

// A generic that's always Transferable is still inferred to be Sendable, even
// when its argument is ~Transferable.
struct Phantom<T: ~Transferable> {}

// Like any other generic type, Box doesn't get a conditional conformance to
// Sendable inferred from its storage.
struct Box<T: ~Transferable>: ~Transferable { var t: T }
// expected-note@-1 2 {{generic struct 'Box' explicitly suppresses conformance to 'Sendable' protocol}}
extension Box: Transferable where T: Transferable {}

// An implicit conformance to Sendable would be unconditional, so a
// conditionally Transferable type doesn't get one even when its storage is
// Sendable.
struct PhantomNT<T: ~Transferable>: ~Transferable {}
// expected-note@-1 {{generic struct 'PhantomNT' explicitly suppresses conformance to 'Sendable' protocol}}
extension PhantomNT: Transferable where T: Transferable {}

func checkInference() {
  takesSendable(NT.self) // expected-error {{type 'NT' does not conform to the 'Sendable' protocol}}
  takesSendable(NTEnum.self) // expected-error {{type 'NTEnum' does not conform to the 'Sendable' protocol}}
  takesSendable(NTClass.self) // expected-error {{type 'NTClass' does not conform to the 'Sendable' protocol}}
  takesSendable(ExplicitNT.self) // expected-error {{type 'ExplicitNT' does not conform to the 'Sendable' protocol}}
  takesSendable(ImplicitNT.self) // expected-error {{type 'ImplicitNT' does not conform to the 'Sendable' protocol}}
  takesSendable(MainActorNT.self) // expected-error {{type 'MainActorNT' does not conform to the 'Sendable' protocol}}
  takesSendable(MainActorNTClass.self) // expected-error {{type 'MainActorNTClass' does not conform to the 'Sendable' protocol}}
  takesSendable(MainActorSubOfNTRoot.self) // expected-error {{type 'MainActorSubOfNTRoot' does not conform to the 'Sendable' protocol}}
  takesSendable(Phantom<Int>.self)
  takesSendable(Phantom<NT>.self)
  takesSendable(Box<Int>.self) // expected-error {{type 'Box<Int>' does not conform to the 'Sendable' protocol}}
  takesSendable(Box<NT>.self) // expected-error {{type 'Box<NT>' does not conform to the 'Sendable' protocol}}
  takesSendable(PhantomNT<Int>.self) // expected-error {{type 'PhantomNT<Int>' does not conform to the 'Sendable' protocol}}
}

// An explicit conformance to Sendable requires Transferable.

struct ExplicitlySendableNT: ~Transferable, Sendable {}
// expected-error@-1 {{type 'ExplicitlySendableNT' does not conform to protocol 'Transferable'}}

struct UncheckedSendableNT: ~Transferable, @unchecked Sendable {}
// expected-error@-1 {{type 'UncheckedSendableNT' does not conform to protocol 'Transferable'}}

// A conditional conformance is fine when its conditions make the type
// Transferable.
struct ConditionallySendable<T: ~Transferable>: ~Transferable { var t: T }
extension ConditionallySendable: Transferable where T: Transferable {}
extension ConditionallySendable: Sendable where T: Sendable {}

struct ExplicitlySendablePhantomNT<T: ~Transferable>: ~Transferable {}
extension ExplicitlySendablePhantomNT: Transferable where T: Transferable {}
extension ExplicitlySendablePhantomNT: Sendable where T: Transferable {}
// expected-note@-1 {{requirement from conditional conformance of 'ExplicitlySendablePhantomNT<NT>' to 'Sendable'}}

func checkExplicit() {
  takesSendable(ConditionallySendable<Int>.self)
  takesSendable(ConditionallySendable<NT>.self) // expected-error {{type 'NT' does not conform to the 'Sendable' protocol}}
  takesSendable(ExplicitlySendablePhantomNT<Int>.self)
  takesSendable(ExplicitlySendablePhantomNT<NT>.self) // expected-error {{global function 'takesSendable' requires that 'NT' conform to 'Transferable'}}
}

// Values of ~Transferable types can't be captured by @Sendable closures.

func useSendable(_ f: @Sendable () -> Void) {}

func checkCaptures(nt: NT, c: NTClass) {
  useSendable { _ = nt } // expected-error {{capture of 'nt' with non-Sendable type 'NT' in a '@Sendable' closure}}
  useSendable { _ = c } // expected-error {{capture of 'c' with non-Sendable type 'NTClass' in a '@Sendable' closure}}
}

// A metatype can be Sendable without values of its type being Transferable,
// so SendableMetatype doesn't imply Transferable, but Sendable still does.

func requiresTransferable<T>(_: T.Type) {}
// expected-note@-1 {{'where T: Transferable' is implicit here}}
func usesSendableMetatype<T: SendableMetatype>(_: T.Type) {}
// expected-note@-1 {{'where T: Transferable' is implicit here}}

func usesNontransferableSendableMetatype<T: SendableMetatype & ~Transferable>(_: T.Type) {
  requiresTransferable(T.self) // expected-error {{global function 'requiresTransferable' requires that 'T' conform to 'Transferable'}}
}

func nontransferableSendable<T: Sendable & ~Transferable>(_: T.Type) {}
// expected-error@-1 {{composition cannot contain '~Transferable' when another member requires 'Transferable'}}

func checkSendableMetatype() {
  usesSendableMetatype(NT.self) // expected-error {{global function 'usesSendableMetatype' requires that 'NT' conform to 'Transferable'}}
  usesNontransferableSendableMetatype(NT.self)
}
