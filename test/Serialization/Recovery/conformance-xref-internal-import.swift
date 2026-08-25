/// A resilient module's PUBLIC conformance is witnessed by a constrained default
/// whose requirement (Tint: CaseIterable) is only reachable through an
/// `internal import`. That dependency is not re-exported, so an out-of-module
/// client that finishes the conformance can't resolve it. This used to abort
/// deserialization (*** DESERIALIZATION FAILURE ***); it must instead recover
/// and emit a normal missing-conformance diagnostic, plus a note pointing the
/// user at the import to add.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Foo \
// RUN:   -o %t/Foo.swiftmodule -swift-version 6 -enable-library-evolution \
// RUN:   -package-name P -I %t
// RUN: %target-swift-frontend -emit-module %t/FooUI.swift -module-name FooUI \
// RUN:   -o %t/FooUI.swiftmodule -swift-version 6 -enable-library-evolution \
// RUN:   -package-name P -I %t
// RUN: %target-swift-frontend -emit-module %t/FooCommands.swift -module-name FooCommands \
// RUN:   -o %t/FooCommands.swiftmodule -swift-version 6 -enable-library-evolution \
// RUN:   -package-name P -enable-testing -I %t

/// The client imports Foo + FooCommands but NOT FooUI. Deserializing the
/// FooCommands witness table cross-references Tint: CaseIterable into FooUI,
/// which the client never loaded. Recover instead of aborting.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -module-name Client \
// RUN:   -swift-version 6 -enable-library-evolution -I %t -verify \
// RUN:   -verify-ignore-unrelated

//--- Foo.swift
public enum Tint {
  case red
  case blue
}

//--- FooUI.swift
import Foo

// Legal same-package retroactive conformance.
extension Tint: CaseIterable {
  public static var allCases: [Tint] { [.red, .blue] }
}

//--- FooCommands.swift
public import Foo
internal import FooUI

public protocol ExpressibleByArgument {
  static var allValueStrings: [String] { get }
}
extension ExpressibleByArgument where Self: CaseIterable {
  public static var allValueStrings: [String] { [] }
}

// Public conformance; its only witness is the CaseIterable-constrained default,
// so the serialized witness carries the conformance Tint: CaseIterable from the
// internally-imported FooUI.
extension Tint: ExpressibleByArgument {}

func useTint() { _ = Tint.allValueStrings }

//--- Client.swift
import Foo
import FooCommands

func useTint() {
  _ = Tint.allValueStrings
  // expected-error@-1 {{static property 'allValueStrings' requires that 'Tint' conform to 'CaseIterable'}}
  // expected-note@-2 {{the conformance of 'Tint' to 'CaseIterable' could not be loaded because module 'FooUI' was not imported; add import of module 'FooUI'}}
}
