// RUN: %empty-directory(%t)

// REQUIRES: asserts

// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-deserialization-safety \
// RUN:   -Xllvm -debug-only=Serialization 2>&1 | %swift-demangle --simplified \
// RUN:   | %FileCheck --check-prefixes=SAFETY-PRIVATE,SAFETY-INTERNAL %s

// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-deserialization-safety \
// RUN:   -Xllvm -debug-only=Serialization \
// RUN:   -enable-testing 2>&1 \
// RUN:   | %FileCheck --check-prefixes=DISABLED %s

/// Don't mark decls as unsafe when private import is enabled.
// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-deserialization-safety \
// RUN:   -Xllvm -debug-only=Serialization \
// RUN:   -enable-private-imports 2>&1 \
// RUN:   | %FileCheck --check-prefixes=DISABLED %s

/// Don't mark decls as unsafe without library evolution.
// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -enable-deserialization-safety -swift-version 5 \
// RUN:   -Xllvm -debug-only=Serialization 2>&1 \
// RUN:   | %FileCheck --check-prefixes=DISABLED %s

// DISABLED-NOT: Serialization safety

public protocol PublicProto {}
// SAFETY-PRIVATE: Serialization safety, safe: 'PublicProto'
internal protocol InternalProto {}
// SAFETY-INTERNAL: Serialization safety, unsafe: 'InternalProto'
// NO-SAFETY-INTERNAL: Serialization safety, unsafe: 'InternalProto'
private protocol PrivateProto {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'PrivateProto'
fileprivate protocol FileprivateProto {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'FileprivateProto'

internal struct InternalStruct : PublicProto {
// SAFETY-INTERNAL: Serialization safety, unsafe: 'InternalStruct'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'InternalStruct'
}

public struct PublicStruct {
// SAFETY-PRIVATE: Serialization safety, safe: 'PublicStruct'

    public typealias publicTypealias = Int
// SAFETY-PRIVATE: Serialization safety, safe: 'publicTypealias'
    typealias internalTypealias = Int
// SAFETY-INTERNAL: Serialization safety, unsafe: 'internalTypealias'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'internalTypealias'
// SAFETY-PRIVATE: Serialization safety, safe: 'localTypealias'

// SAFETY-PRIVATE: Serialization safety, safe: 'opaque PublicStruct.opaqueTypeFunc()'
// SAFETY-PRIVATE: Serialization safety, unsafe: 'opaque PublicStruct.opaqueFuncInternal()'

    public init(publicInit a: Int) {}
// SAFETY-PRIVATE: Serialization safety, safe: 'init(publicInit:)'
    internal init(internalInit a: Int) {}
// SAFETY-INTERNAL: Serialization safety, unsafe: 'init(internalInit:)'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'init(internalInit:)'
    private init(privateInit a: Int) {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'init(privateInit:)'
    fileprivate init(fileprivateInit a: Int) {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'init(fileprivateInit:)'

    @inlinable public func inlinableFunc() {
// SAFETY-PRIVATE: Serialization safety, safe: 'inlinableFunc()'
        typealias localTypealias = Int

        func inlinableFunc_nested() {}
// SAFETY-PRIVATE-NOT: inlinableFunc_nested()
        inlinableFunc_nested()
    }

    public func publicFunc() {
// SAFETY-PRIVATE: Serialization safety, safe: 'publicFunc()'
        func publicFunc_nested() {}
// SAFETY-PRIVATE-NOT: publicFunc_nested()
        publicFunc_nested()
    }

    @available(SwiftStdlib 5.1, *) // for the `some` keyword.
    public func opaqueTypeFunc() -> some PublicProto {
// SAFETY-PRIVATE: Serialization safety, safe: 'opaqueTypeFunc()'
        return InternalStruct()
    }

    @available(SwiftStdlib 5.1, *) // for the `some` keyword.
    internal func opaqueFuncInternal() -> some PublicProto {
// SAFETY-PRIVATE: Serialization safety, unsafe: 'opaqueFuncInternal()'
        return InternalStruct()
    }

    internal func internalFunc() {}
// SAFETY-INTERNAL: Serialization safety, unsafe: 'internalFunc()'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'internalFunc()'
    private func privateFunc() {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'privateFunc()'
    fileprivate func fileprivateFunc() {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'fileprivateFunc()'

    public var publicProperty = "Some text"
// SAFETY-PRIVATE: Serialization safety, safe: 'publicProperty'
    public private(set) var halfPublicProperty = "Some text"
// SAFETY-PRIVATE: Serialization safety, safe: 'halfPublicProperty'
    private var privateProperty = "Some text"
// SAFETY-PRIVATE: Serialization safety, unsafe: 'privateProperty'
}
