// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: asserts
// REQUIRES: VENDOR=apple

/// Build library.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule

/// Build client.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -enable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck --check-prefixes=SAFE %s

/// Decls skips by the deserialization safety logic.
// SAFE-NOT: InternalClass
// SAFE: Deserialized: 'PublicClass'
// SAFE: Deserialized: 'publicFunc()'
// SAFE: Skipping unsafe deserialization: 'privateFunc()'
// SAFE: Skipping unsafe deserialization: 'fileprivateFunc()'
// SAFE: Skipping unsafe deserialization: 'internalFunc()'

//--- Lib.swift

import Foundation

@objc
public class PublicClass : NSObject {
    public func publicFunc() {}
}

@objc
internal class InternalClass : NSObject {
    private func privateFunc() {}
    fileprivate func fileprivateFunc() {}
    internal func internalFunc() {}
}

//--- Client.swift

import Lib

var x: AnyObject

// Trigger a typo correction to read all members of all subtypes to NSObject.
x.notAMember() // expected-error {{value of type 'AnyObject' has no member 'notAMember'}}
