// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build three separate modules: Core (declares the protocol and the
// protocol-extension type alias that witnesses the associated type), Mid (the
// conformer, which does *not* declare the associated type explicitly), and App
// (which references the conformer's associated type and imports both modules).

// RUN: %target-swift-frontend -emit-module -module-name Core %t/Core.swift -emit-module-path %t/Core.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name Mid %t/Mid.swift -emit-module-path %t/Mid.swiftmodule -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/App.swift -I %t

// https://github.com/swiftlang/swift/issues/89909
//
// Cross-module qualified lookup of an associated type whose witness is supplied
// by a type alias in a protocol extension used to fail when the protocol
// extension, the conformer, and the reference site each lived in a separate
// module. Deserializing the conformer's module used to drop the witness type
// alias declaration (because the generic type alias lives in yet another
// module), which left qualified lookup with no declaration to resolve, so
// 'Foo.Action' failed with "'Action' is not a member type of struct 'Foo'".

//--- Core.swift

public protocol Feature {
    associatedtype State
    associatedtype Action
}

extension Feature {
    public typealias Action = GenericAction<Self, State>
}

public enum GenericAction<F: Feature, State> {
    case state(State)
}

//--- Mid.swift

import Core

public struct Foo: Feature {
    public typealias State = Int
}

//--- App.swift

import Core
import Mid

// The associated type 'Action' resolves to the extension-supplied witness
// 'GenericAction<Foo, Int>'.
public func useFooAction(_ action: Foo.Action) {}

func checkWitnessType() {
    let _: Foo.Action = GenericAction<Foo, Int>.state(0)
}
