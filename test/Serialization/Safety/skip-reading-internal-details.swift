// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: asserts

/// Build libraries.
// RUN: %target-swift-frontend -emit-module %t/HiddenLib.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/HiddenLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

/// Build clients, with and without safety.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck --check-prefixes=NEEDED,UNSAFE %s

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-access-control 2>&1 \
// RUN:   | %FileCheck --check-prefixes=NEEDED,UNSAFE %s

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -enable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck --check-prefixes=NEEDED,CLEAN,SAFE %s

/// Disabled by default.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck --check-prefixes=NEEDED,UNSAFE %s

/// Enable with env var.
// RUN: env SWIFT_ENABLE_DESERIALIZATION_SAFETY=true \
// RUN:   %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization 2>&1 \
// RUN:   | %FileCheck --check-prefixes=NEEDED,CLEAN,SAFE %s

/// Build against the swiftinterface.
// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck --check-prefixes=NEEDED,CLEAN %s

/// Decls part of the API needed by the client.
// NEEDED-NOT: Deserialized: 'refToIOI()'
// NEEDED: Deserialized: 'PublicStruct'
// NEEDED: Deserialized: 'publicFunc()'

/// Internal details dangerous to load.
// UNSAFE: Deserialized: 'internalFunc()'
// UNSAFE: Deserialized: 'privateFunc()'
// UNSAFE: Deserialized: 'fileprivateFunc()'

/// Decls removed by rebuilding from the swiftinterface.
// CLEAN-NOT: Deserialized: 'internalFunc()'
// CLEAN-NOT: Deserialized: 'privateFunc()'
// CLEAN-NOT: Deserialized: 'fileprivateFunc()'

/// Decls skips by the deserialization safety logic.
// SAFE: Skipping unsafe deserialization: 'internalFunc()'
// SAFE: Skipping unsafe deserialization: 'privateFunc()'
// SAFE: Skipping unsafe deserialization: 'fileprivateFunc()'
// SAFE: Skipping unsafe deserialization: 'refToIOI()'

//--- HiddenLib.swift

public struct HiddenStruct {
    public init() {}
}

//--- Lib.swift

@_implementationOnly import HiddenLib

public struct PublicStruct {
    public init() {}

    public func publicFunc() {}
    internal func internalFunc() {}
    private func privateFunc() {}
    fileprivate func fileprivateFunc() {}

    internal func refToIOI() -> HiddenStruct {
        return HiddenStruct();
    }
}

// resultBuilder scenario
public protocol V {}

@resultBuilder
public struct VB {
    public static func buildExpression<Content>(_ content: Content) -> Content where Content : V { fatalError() }
    public static func buildBlock() -> V { fatalError() }
    public static func buildBlock<Content>(_ content: Content) -> Content where Content : V { fatalError() }
}

public struct EV : V {
    public init () {}
}

@available(SwiftStdlib 5.1, *)
public extension V {
  @VB
  func opaqueReferencingPrivate() -> some V {
    referencedPrivateFunc(v: EV())
  }

  private func referencedPrivateFunc(v: some V) -> some V { return v }
}

//--- Client.swift

import Lib

var x = PublicStruct()

// Trigger a typo correction that reads all members.
x.notAMember() // expected-error {{value of type 'PublicStruct' has no member 'notAMember'}}

if #available(SwiftStdlib 5.1, *) {
  let v = EV()
  let _ = v.opaqueReferencingPrivate()
}
