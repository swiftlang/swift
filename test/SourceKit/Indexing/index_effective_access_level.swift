// RUN: %empty-directory(%t)
// RUN: %swift -emit-module -o %t/Exported.swiftmodule %S/Inputs/explicit-access/Exported.swift
// RUN: %swift -emit-module -o %t/Module.swiftmodule %S/Inputs/explicit-access/Module.swift -I %t

// RUN: %sourcekitd-test -req=index %s -- -I %t %s | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

public enum PublicEnum {
    case publicEnumCase
}

enum InternalEnum {
    case internalEnumCase
}

fileprivate enum FilePrivateEnum {
    case filePrivateEnumCase
}

private enum PrivateEnum {
    case privateEnumCase
}

extension PublicEnum {
    public func publicMethod() {}
}

public extension PublicEnum {
    func methodFromPublicExtension() {}
}

extension InternalEnum {
    func internalMethod() {}
}

extension FilePrivateEnum {
    fileprivate func filePrivateMethod() {}
}

fileprivate extension FilePrivateEnum {
    func methodFromFilePrivateExtension() {}
}

extension PrivateEnum {
    private func privateMethod() {}
}

private extension PrivateEnum {
    func methodFromPrivateExtension() {}
}

@propertyWrapper
public struct PublicPropertyWrapper<T> {
    public var wrappedValue: T
    public init(wrappedValue: T) {
        self.wrappedValue = wrappedValue
    }
}

@propertyWrapper
struct InternalPropertyWrapper<T> {
    let wrappedValue: T
}

@propertyWrapper
fileprivate struct FilePrivatePropertyWrapper<T> {
    fileprivate let wrappedValue: T
}

@propertyWrapper
private struct PrivatePropertyWrapper<T> {
    private let wrappedValue: T
}

private struct ScopeReducerStruct {
    public init(publicInitializer: Int) {}
    init(internalInitializer: Int) {}
    fileprivate init(filePrivateInitializer: Int) {}
    private init(privateInitializer: Int) {}

    public let publicProperty: Int = 0
    let internalProperty: Int = 0
    fileprivate let filePrivateProperty: Int = 0
    private let privateProperty: Int = 0
    public private(set) var publicPropertyWithPrivateSetter: Int = 0

    @PublicPropertyWrapper public var publicPropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper var internalPropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper fileprivate var filePrivatePropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper private var privatePropertyWrappedProperty: Int = 0

    public subscript(publicSubscript: Int) -> Int { return 0 }
    subscript(internalSubscript: Int) -> Int { return 0 }
    fileprivate subscript(filePrivateSubscript: Int) -> Int { return 0 }
    private subscript(privateSubscript: Int) -> Int { return 0 }

    public func publicMethod() {}
    func internalMethod() {}
    fileprivate func filePrivateMethod() {}
    private func privateMethod() {}
}

public struct ScopeKeeperStruct {
    public init(publicInitializer: Int) {}
    init(internalInitializer: Int) {}
    fileprivate init(filePrivateInitializer: Int) {}
    private init(privateInitializer: Int) {}

    public let publicProperty: Int = 0
    let internalProperty: Int = 0
    fileprivate let filePrivateProperty: Int = 0
    private let privateProperty: Int = 0
    public private(set) var publicPropertyWithPrivateSetter: Int = 0

    @PublicPropertyWrapper public var publicPropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper var internalPropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper fileprivate var filePrivatePropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper private var privatePropertyWrappedProperty: Int = 0

    public subscript(publicSubscript: Int) -> Int { return 0 }
    subscript(internalSubscript: Int) -> Int { return 0 }
    fileprivate subscript(filePrivateSubscript: Int) -> Int { return 0 }
    private subscript(privateSubscript: Int) -> Int { return 0 }

    public func publicMethod() {}
    func internalMethod() {}
    fileprivate func filePrivateMethod() {}
    private func privateMethod() {}
}

struct PartialScopeReducerStruct {
    public init(publicInitializer: Int) {}
    init(internalInitializer: Int) {}
    fileprivate init(filePrivateInitializer: Int) {}
    private init(privateInitializer: Int) {}

    public let publicProperty: Int = 0
    let internalProperty: Int = 0
    fileprivate let filePrivateProperty: Int = 0
    private let privateProperty: Int = 0
    public private(set) var publicPropertyWithPrivateSetter: Int = 0

    @PublicPropertyWrapper public var publicPropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper var internalPropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper fileprivate var filePrivatePropertyWrappedProperty: Int = 0
    @PublicPropertyWrapper private var privatePropertyWrappedProperty: Int = 0

    public subscript(publicSubscript: Int) -> Int { return 0 }
    subscript(internalSubscript: Int) -> Int { return 0 }
    fileprivate subscript(filePrivateSubscript: Int) -> Int { return 0 }
    private subscript(privateSubscript: Int) -> Int { return 0 }

    public func publicMethod() {}
    func internalMethod() {}
    fileprivate func filePrivateMethod() {}
    private func privateMethod() {}
}

private extension PrivateEnum {
    private func privateMethodFromPrivateExtension() {}
}

public protocol PublicProtocol {
    var member: Int { get set }
    func method()
}

protocol InternalProtocol {
    var member: Int { get set }
    func method()
}

fileprivate protocol FilePrivateProtocol {
    var member: Int { get set }
    func method()
}

private protocol PrivateProtocol {
    var member: Int { get set }
    func method()
}

fileprivate struct FilePrivateImplementationOfPublicProtocol: PublicProtocol {
    fileprivate var member: Int = 0
    fileprivate func method() {}
}

open class OpenClass {
    open var openProperty: Int { return 0 }
    public var publicProperty: Int { return 0 }
    var internalProperty: Int { return 0 }

    open func openMethod() {}
    public func publicMethod() {}
    func internalMethod() {}
}

import Module

struct InternalStruct {
    let propertyReferencingPublicClassFromModule: Module.ModuleClass
    let propertyReferencingPublicClassFromExportedModule: Exported.ExportedClass
}

public typealias Alias = Int

public var globalVariable: Int = 0

protocol ProtocolWithAssociatedType {
    associatedtype T
}

struct ProtocolWithAssociatedTypeImpl: ProtocolWithAssociatedType {
    typealias T = Int
    func testLocalContent() {
        let localVariableShouldntBeIndexed = 0
    }
}
