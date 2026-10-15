// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t %t/Lib.swift -enable-experimental-feature NontransferableTypes
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/main.swift -enable-experimental-feature NontransferableTypes
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/main.swift -enable-experimental-feature NontransferableTypes -enable-experimental-feature MoveOnlyClasses

// REQUIRES: objc_interop
// REQUIRES: swift_feature_NontransferableTypes
// REQUIRES: swift_feature_MoveOnlyClasses

//--- Lib.swift
open class LibNonTransferable: ~Transferable {
  public init() {}
}

open class LibTransferable {
  public init() {}
}

//--- main.swift
import ObjectiveC
import Lib

struct NonTransferable: ~Transferable {}
// expected-note@-1 2 {{struct 'NonTransferable' has '~Transferable' constraint preventing 'Transferable' conformance}}

func requiresTransferable<T>(_: T.Type) {}
// expected-note@-1 3 {{'where T: Transferable' is implicit here}}

// Root classes

class Root: ~Transferable {}
// expected-note@-1 {{class 'Root' has '~Transferable' constraint preventing 'Transferable' conformance}}

struct HoldsRoot { // expected-note {{consider adding '~Transferable' to struct 'HoldsRoot'}}
  var root: Root // expected-error {{stored property 'root' of 'Transferable'-conforming struct 'HoldsRoot' has non-'Transferable' type 'Root'}}
}

class RootButTransferable: ~Transferable, Transferable {}
// expected-error@-1 {{class 'RootButTransferable' required to be 'Transferable' but is marked with '~Transferable'}}

class RootHoldsNonTransferable: ~Transferable {
  var nt: NonTransferable
  init(nt: NonTransferable) { self.nt = nt }
}

class TransferableHoldsNonTransferable { // expected-note {{consider adding '~Transferable' to class 'TransferableHoldsNonTransferable'}}
  var nt: NonTransferable // expected-error {{stored property 'nt' of 'Transferable'-conforming class 'TransferableHoldsNonTransferable' has non-'Transferable' type 'NonTransferable'}}
  init(nt: NonTransferable) { self.nt = nt }
}

// Subclasses inherit Transferable-ness from their superclass.

class SubOfRoot: Root {}
class RestatesInverse: Root, ~Transferable {} // Ok (but redundant)

class ExplicitlyTransferableSub: Root, Transferable {}
// expected-error@-1 {{conformance of class 'ExplicitlyTransferableSub' to 'Transferable' must be inherited from its superclass 'Root'}}

class TransferableInExtensionSub: Root {}
extension TransferableInExtensionSub: Transferable {}
// expected-error@-1 {{conformance of class 'TransferableInExtensionSub' to 'Transferable' must be inherited from its superclass 'Root'}}

class TransferableBase {
  init() {}
}

class RedundantlyTransferableSub: TransferableBase, Transferable {}
// expected-warning@-1 {{redundant conformance of 'RedundantlyTransferableSub' to protocol 'Transferable'}}
// expected-note@-2 {{'RedundantlyTransferableSub' inherits conformance to protocol 'Transferable' from superclass here}}

class NonTransferableSub: TransferableBase, ~Transferable {}
// expected-error@-1 {{class 'NonTransferableSub' cannot be '~Transferable' because its superclass 'TransferableBase' can be 'Transferable'}}

class SubHoldsNonTransferable: TransferableBase {
  var nt: NonTransferable // expected-error {{stored property 'nt' of 'Transferable'-conforming class 'SubHoldsNonTransferable' has non-'Transferable' type 'NonTransferable'}}
  init(nt: NonTransferable) { self.nt = nt }
}

class SubOfRootHoldsNonTransferable: Root {
  var nt: NonTransferable
  init(nt: NonTransferable) { self.nt = nt }
}

class Conditional<T: ~Transferable>: ~Transferable {}
extension Conditional: Transferable where T: Transferable {}

class SubOfConditionalInt: Conditional<Int> {}
class SubOfConditionalNonTransferable: Conditional<NonTransferable> {}
// expected-note@-1 {{requirement from conditional conformance of 'SubOfConditionalNonTransferable' to 'Transferable'}}

class NonTransferableSubOfConditional: Conditional<Int>, ~Transferable {}
// expected-error@-1 {{class 'NonTransferableSubOfConditional' cannot be '~Transferable' because its superclass 'Conditional' can be 'Transferable'}}

class SubOfLibNonTransferable: LibNonTransferable {}

class NonTransferableSubOfLib: LibTransferable, ~Transferable {}
// expected-error@-1 {{class 'NonTransferableSubOfLib' cannot be '~Transferable' because its superclass 'LibTransferable' can be 'Transferable'}}

class NonTransferableObject: NSObject, ~Transferable {}
// expected-error@-1 {{class 'NonTransferableObject' cannot be '~Transferable' because its superclass 'NSObject' can be 'Transferable'}}

func checkGenericArguments() {
  requiresTransferable(Root.self) // expected-error {{global function 'requiresTransferable' requires that 'Root' conform to 'Transferable'}}
  requiresTransferable(SubOfRoot.self) // expected-error {{global function 'requiresTransferable' requires that 'SubOfRoot' conform to 'Transferable'}}
  requiresTransferable(TransferableBase.self)
  requiresTransferable(SubHoldsNonTransferable.self)
  requiresTransferable(SubOfConditionalInt.self)
  requiresTransferable(SubOfConditionalNonTransferable.self) // expected-error {{global function 'requiresTransferable' requires that 'NonTransferable' conform to 'Transferable'}}
  requiresTransferable(LibTransferable.self)
  requiresTransferable(SubOfLibNonTransferable.self) // expected-error {{global function 'requiresTransferable' requires that 'SubOfLibNonTransferable' conform to 'Transferable'}}
  requiresTransferable(NSObject.self)
  requiresTransferable(NonTransferableObject.self)
}

// Actors are Sendable and Sendable implies Transferable.

actor NonTransferableActor: ~Transferable {} // expected-error {{actors cannot be '~Transferable'}}

actor ActorHoldsNonTransferable {
  var nt: NonTransferable // Ok
  init(nt: NonTransferable) { self.nt = nt }
}

func checkActors() {
  requiresTransferable(NonTransferableActor.self)
  requiresTransferable(ActorHoldsNonTransferable.self)
}
