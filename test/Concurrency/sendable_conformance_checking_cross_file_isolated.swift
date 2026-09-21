// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/Definitions.swift %t/Conformance.swift -swift-version 5
// RUN: %target-swift-frontend -typecheck -verify %t/Definitions.swift %t/Conformance.swift -swift-version 5 -strict-concurrency=complete
// RUN: %target-swift-frontend -typecheck -verify %t/Definitions.swift %t/Conformance.swift -swift-version 6

// REQUIRES: concurrency

//--- Definitions.swift
class NonSendable {}

protocol RefinesSendable: Sendable {}

@MainActor
class Extends: NonSendable {}
// expected-note@-1:16 {{inherits from non-Sendable class 'NonSendable'}}

@MainActor
class Refined: NonSendable {}
// expected-note@-1:16 {{inherits from non-Sendable class 'NonSendable'}}

@MainActor
class IsolatedBase: NonSendable {}

// '@MainActor' inherited from the superclass, not stated.
class InheritsIsolation: IsolatedBase {}
// expected-note@-1:26 {{inherits from non-Sendable class 'IsolatedBase'}}

//--- Conformance.swift
extension Extends: Sendable {}
// expected-warning@-1:1 {{conformance to 'Sendable' must occur in the same source file as class 'Extends'; use '@unchecked Sendable' for retroactive conformance; this will be an error in a future Swift language mode}}
// expected-warning@-2:1 {{class 'Extends' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-3:1 {{a 'Sendable' class cannot inherit from a non-Sendable class}}

extension InheritsIsolation: Sendable {}
// expected-warning@-1:1 {{conformance to 'Sendable' must occur in the same source file as class 'InheritsIsolation'; use '@unchecked Sendable' for retroactive conformance; this will be an error in a future Swift language mode}}
// expected-warning@-2:1 {{class 'InheritsIsolation' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-3:1 {{a 'Sendable' class cannot inherit from a non-Sendable class}}

extension Refined: RefinesSendable {}
// expected-warning@-1:1 {{conformance to 'Sendable' must occur in the same source file as class 'Refined'; use '@unchecked Sendable' for retroactive conformance; this will be an error in a future Swift language mode}}
// expected-warning@-2:1 {{class 'Refined' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-3:1 {{a 'Sendable' class cannot inherit from a non-Sendable class}}

func requiresSendable<T: Sendable>(_: T.Type) {}
func testStillSendable() {
  requiresSendable(Extends.self)
  requiresSendable(InheritsIsolation.self)
  requiresSendable(Refined.self)
}
