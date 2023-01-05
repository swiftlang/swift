// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: asserts

// RUN: %target-swift-frontend -emit-module %t/HiddenLib.swift \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/HiddenLib.swiftmodule

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization 2>&1 \
// RUN:   | %FileCheck --check-prefixes=NEEDED,UNSAFE %s

// RUN: rm %t/Lib.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization 2>&1 \
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

//--- HiddenLib.swift

public struct HiddenStruct {
    public init() {}
}

//--- Lib.swift

@_implementationOnly import HiddenLib

public class PublicStruct {
    public init() {}

    public func publicFunc() {}
    internal func internalFunc() {}
    private func privateFunc() {}
    fileprivate func fileprivateFunc() {}

    internal func refToIOI() -> HiddenStruct {
        return HiddenStruct();
    }
}

//--- Client.swift

import Lib

var x = PublicStruct()

// Trigger a typo correction that reads all members.
x.notAMember() // expected-error {{value of type 'PublicStruct' has no member 'notAMember'}}
