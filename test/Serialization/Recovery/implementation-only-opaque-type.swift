// Test deserialization when the underlying type of an opaque type
// depends on an implementation-only import.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/BaseLib.swiftmodule %t/BaseLib.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/HiddenLib.swiftmodule %t/HiddenLib.swift -I %t
// RUN: %target-swift-frontend -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/Lib.swiftmodule %t/Lib.swift -I %t

// RUN: %target-swift-frontend -I %t -emit-ir %t/Client.swift

//--- BaseLib.swift

public protocol Proto { }

//--- HiddenLib.swift

import BaseLib

public struct HiddenType : Proto {
    public init() {}
}

//--- Lib.swift

import BaseLib
@_implementationOnly import HiddenLib

public struct PublicStruct {
    public init() {}
    public func foo() -> some Proto {
        return HiddenType()
    }
}

//--- Client.swift

import Lib

var s = PublicStruct()
let r = s.foo()
