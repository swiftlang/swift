// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t \
// RUN:   -enable-library-evolution

/// Check diagnostics.
// RUN: %target-swift-frontend -typecheck %t/MinimalClient.swift -I %t \
// RUN:   -package-name TestPackage -swift-version 5 -verify
// RUN: %target-swift-frontend -typecheck %t/CompletenessClient.swift -I %t \
// RUN:   -package-name TestPackage -swift-version 5 -verify

/// Check diagnostics with library-evolution.
// RUN: %target-swift-frontend -typecheck %t/MinimalClient.swift -I %t \
// RUN:   -package-name TestPackage -swift-version 5 \
// RUN:   -enable-library-evolution -verify
// RUN: %target-swift-frontend -typecheck %t/CompletenessClient.swift -I %t \
// RUN:   -package-name TestPackage -swift-version 5 \
// RUN:   -enable-library-evolution -verify

//--- PublicLib.swift
public protocol PublicImportProto {
    associatedtype T
}
public struct PublicImportType {
    public init() {}
}
open class PublicImportClass {}

@propertyWrapper
public struct PublicLibWrapper<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

//--- PackageLib.swift
public protocol PackageImportProto {
    associatedtype T
}
public struct PackageImportType {
    public init() {}
}
open class PackageImportClass {}

@propertyWrapper
public struct PackageLibWrapper<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

//--- InternalLib.swift
public protocol InternalImportProto {
    associatedtype T
}
public struct InternalImportType {
    public init() {}
}
open class InternalImportClass {}
public func InternalFunc() {}

@propertyWrapper
public struct InternalLibWrapper<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

//--- FileprivateLib.swift
public protocol FileprivateImportProto {
    associatedtype T
}
public struct FileprivateImportType {
    public init() {}
}
open class FileprivateImportClass {}

@propertyWrapper
public struct FileprivateLibWrapper<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

//--- PrivateLib.swift
public protocol PrivateImportProto {
    associatedtype T
}
public struct PrivateImportType {
    public init() {}
}
open class PrivateImportClass {}

@propertyWrapper
public struct PrivateLibWrapper<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

/// Short test mostly to count the notes.
//--- MinimalClient.swift
public import PublicLib // expected-warning {{public import of 'PublicLib' was not used in public declarations or inlinable code}}
package import PackageLib // expected-warning {{package import of 'PackageLib' was not used in package declarations}}
internal import InternalLib // expected-note {{struct 'InternalImportType' imported as 'internal' from 'InternalLib' here}}
fileprivate import FileprivateLib // expected-note {{class 'FileprivateImportClass' imported as 'fileprivate' from 'FileprivateLib' here}}
private import PrivateLib

public func PublicFuncUsesInternal(_: InternalImportType) { // expected-error {{function cannot be declared public because its parameter uses an internal type}}
// expected-note @-1 {{struct 'InternalImportType' is imported by this file as 'internal' from 'InternalLib'}}
    var _: InternalImportType
}

public class PublicSubclassFileprivate : FileprivateImportClass {} // expected-error {{class cannot be declared public because its superclass is fileprivate}}
// expected-note @-1 {{class 'FileprivateImportClass' is imported by this file as 'fileprivate' from 'FileprivateLib'}}

/// More complete test.
//--- CompletenessClient.swift
public import PublicLib
package import PackageLib // expected-note * {{module 'PackageLib' imported as 'package' here}}
// expected-note@-1 * {{imported as 'package' from 'PackageLib' here}}
internal import InternalLib // expected-note * {{module 'InternalLib' imported as 'internal' here}}
// expected-note@-1 * {{imported as 'internal' from 'InternalLib' here}}
fileprivate import FileprivateLib // expected-note * {{module 'FileprivateLib' imported as 'fileprivate' here}}
// expected-note@-1 * {{imported as 'fileprivate' from 'FileprivateLib' here}}
private import PrivateLib // expected-note * {{module 'PrivateLib' imported as 'private' here}}
// expected-note@-1 * {{imported as 'private' from 'PrivateLib' here}}

// Public use sites
public func PublicFuncUsesPublic(_: PublicImportType) {
    var _: PublicImportType
}
public func PublicFuncUsesPackage(_: PackageImportType) { // expected-error {{function cannot be declared public because its parameter uses a package type}}
// expected-note @-1 {{is imported by this file as}}
    var _: PackageImportType
}
public func PublicFuncUsesInternal(_: InternalImportType) { // expected-error {{function cannot be declared public because its parameter uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    var _: InternalImportType
}
public func PublicFuncUsesFileprivate(_: FileprivateImportType) { // expected-error {{function cannot be declared public because its parameter uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    var _: FileprivateImportType
}
public func PublicFuncUsesPrivate(_: PrivateImportType) { // expected-error {{function cannot be declared public because its parameter uses a private type}}
// expected-note @-1 {{is imported by this file as}}
    var _: PrivateImportType
}

// Package use sites
package func PackageFuncUsesPublic(_: PublicImportType) {
    var _: PublicImportType
}
package func PackageFuncUsesPackage(_: PackageImportType) {
    var _: PackageImportType
}
package func PackageFuncUsesInternal(_: InternalImportType) { // expected-error {{function cannot be declared package because its parameter uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    var _: InternalImportType
}
package func PackageFuncUsesFileprivate(_: FileprivateImportType) { // expected-error {{function cannot be declared package because its parameter uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    var _: FileprivateImportType
}
package func PackageFuncUsesPrivate(_: PrivateImportType) { // expected-error {{function cannot be declared package because its parameter uses a private type}}
// expected-note @-1 {{is imported by this file as}}
    var _: PrivateImportType
}

// Internal use sites
internal func InternalFuncUsesPublic(_: PublicImportType) {
    var _: PublicImportType
}
internal func InternalFuncUsesPackage(_: PackageImportType) {
    var _: PackageImportType
}
internal func InternalFuncUsesInternal(_: InternalImportType) {
    var _: InternalImportType
}
internal func InternalFuncUsesFileprivate(_: FileprivateImportType) { // expected-error {{function cannot be declared internal because its parameter uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    var _: FileprivateImportType
}
internal func InternalFuncUsesPrivate(_: PrivateImportType) { // expected-error {{function cannot be declared internal because its parameter uses a private type}}
// expected-note @-1 {{is imported by this file as}}
    var _: PrivateImportType
}

// Fileprivate use sites
fileprivate func FileprivateFuncUsesPublic(_: PublicImportType) {
    var _: PublicImportType
}
fileprivate func FileprivateFuncUsesPackage(_: PackageImportType) {
    var _: PackageImportType
}
fileprivate func FileprivateFuncUsesInternal(_: InternalImportType) {
    var _: InternalImportType
}
fileprivate func FileprivateFuncUsesFileprivate(_: FileprivateImportType) {
    var _: FileprivateImportType
}
fileprivate func FileprivateFuncUsesPrivate(_: PrivateImportType) {
    var _: PrivateImportType
}

// Private use sites
private func PrivateFuncUsesPublic(_: PublicImportType) {
    var _: PublicImportType
}
private func PrivateFuncUsesPackage(_: PackageImportType) {
    var _: PackageImportType
}
private func PrivateFuncUsesInternal(_: InternalImportType) {
    var _: InternalImportType
}
private func PrivateFuncUsesFileprivate(_: FileprivateImportType) {
    var _: FileprivateImportType
}
private func PrivateFuncUsesPrivate(_: PrivateImportType) {
    var _: PrivateImportType
}
// Public returns
public func PublicFuncReturnUsesPublic() -> PublicImportType {
    fatalError()
}
public func PublicFuncReturnUsesPackage() -> PackageImportType { // expected-error {{function cannot be declared public because its result uses a package type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}
public func PublicFuncReturnUsesInternal() -> InternalImportType { // expected-error {{function cannot be declared public because its result uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}
public func PublicFuncReturnUsesFileprivate() -> FileprivateImportType { // expected-error {{function cannot be declared public because its result uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}
public func PublicFuncReturnUsesPrivate() -> PrivateImportType { // expected-error {{function cannot be declared public because its result uses a private type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}

// Package returns
package func PackageFuncReturnUsesPublic() -> PublicImportType {
    fatalError()
}
package func PackageFuncReturnUsesPackage() -> PackageImportType {
    fatalError()
}
package func PackageFuncReturnUsesInternal() -> InternalImportType { // expected-error {{function cannot be declared package because its result uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}
package func PackageFuncReturnUsesFileprivate() -> FileprivateImportType { // expected-error {{function cannot be declared package because its result uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}
package func PackageFuncReturnUsesPrivate() -> PrivateImportType { // expected-error {{function cannot be declared package because its result uses a private type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}

// Internal returns
internal func InternalFuncReturnUsesPublic() -> PublicImportType {
    fatalError()
}
internal func InternalFuncReturnUsesPackage() -> PackageImportType {
    fatalError()
}
internal func InternalFuncReturnUsesInternal() -> InternalImportType {
    fatalError()
}
internal func InternalFuncReturnUsesFileprivate() -> FileprivateImportType { // expected-error {{function cannot be declared internal because its result uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}
internal func InternalFuncReturnUsesPrivate() -> PrivateImportType { // expected-error {{function cannot be declared internal because its result uses a private type}}
// expected-note @-1 {{is imported by this file as}}
    fatalError()
}

// Fileprivate returns
fileprivate func FileprivateFuncReturnUsesPublic() -> PublicImportType {
    fatalError()
}
fileprivate func FileprivateFuncReturnUsesPackage() -> PackageImportType {
    fatalError()
}
fileprivate func FileprivateFuncReturnUsesInternal() -> InternalImportType {
    fatalError()
}
fileprivate func FileprivateFuncReturnUsesFileprivate() -> FileprivateImportType {
    fatalError()
}
fileprivate func FileprivateFuncReturnUsesPrivate() -> PrivateImportType {
    fatalError()
}

// Private returns
private func PrivateFuncReturnUsesPublic() -> PublicImportType {
    fatalError()
}
private func PrivateFuncReturnUsesPackage() -> PackageImportType {
    fatalError()
}
private func PrivateFuncReturnUsesInternal() -> InternalImportType {
    fatalError()
}
private func PrivateFuncReturnUsesFileprivate() -> FileprivateImportType {
    fatalError()
}
private func PrivateFuncReturnUsesPrivate() -> PrivateImportType {
    fatalError()
}

// Public subscripts
public struct PublicSubscriptUsesPublic {
    public subscript(index: PublicImportType) -> PublicImportType {
        fatalError()
    }
}
public struct PublicSubscriptUsesPackage {
    public subscript(index: PackageImportType) -> PackageImportType { // expected-error {{subscript cannot be declared public because its element type uses a package type}}
// expected-note @-1 {{is imported by this file as}}
    // This error should be on the `index` like the other ones.
        fatalError()
    }
}
public struct PublicSubscriptUsesInternal {
    public subscript(index: InternalImportType) -> InternalImportType { // expected-error {{subscript cannot be declared public because its index uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
        fatalError()
    }
}
public struct PublicSubscriptUsesFileprivate {
    public subscript(index: FileprivateImportType) -> FileprivateImportType { // expected-error {{subscript cannot be declared public because its index uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
        fatalError()
    }
}
public struct PublicSubscriptUsesPrivate {
    public subscript(index: PrivateImportType) -> PrivateImportType { // expected-error {{subscript cannot be declared public because its index uses a private type}}
// expected-note @-1 {{is imported by this file as}}
        fatalError()
    }
}

// Package subscripts
package struct PackageSubscriptUsesPublic {
    package subscript(index: PublicImportType) -> PublicImportType {
        fatalError()
    }
}
package struct PackageSubscriptUsesPackage {
    package subscript(index: PackageImportType) -> PackageImportType {
        fatalError()
    }
}
package struct PackageSubscriptUsesInternal {
    package subscript(index: InternalImportType) -> InternalImportType { // expected-error {{subscript cannot be declared package because its index uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
        fatalError()
    }
}
package struct PackageSubscriptUsesFileprivate {
    package subscript(index: FileprivateImportType) -> FileprivateImportType { // expected-error {{subscript cannot be declared package because its index uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
        fatalError()
    }
}
package struct PackageSubscriptUsesPrivate {
    package subscript(index: PrivateImportType) -> PrivateImportType { // expected-error {{subscript cannot be declared package because its index uses a private type}}
// expected-note @-1 {{is imported by this file as}}
        fatalError()
    }
}

// Internal subscripts
internal struct InternalSubscriptUsesPublic {
    internal subscript(index: PublicImportType) -> PublicImportType {
        fatalError()
    }
}
internal struct InternalSubscriptUsesPackage {
    internal subscript(index: PackageImportType) -> PackageImportType {
        fatalError()
    }
}
internal struct InternalSubscriptUsesInternal {
    internal subscript(index: InternalImportType) -> InternalImportType {
        fatalError()
    }
}
internal struct InternalSubscriptUsesFileprivate {
    internal subscript(index: FileprivateImportType) -> FileprivateImportType { // expected-error {{subscript cannot be declared internal because its index uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
        fatalError()
    }
}
internal struct InternalSubscriptUsesPrivate {
    internal subscript(index: PrivateImportType) -> PrivateImportType { // expected-error {{subscript cannot be declared internal because its index uses a private type}}
// expected-note @-1 {{is imported by this file as}}
        fatalError()
    }
}

// Fileprivate subscripts
fileprivate struct FileprivateSubscriptUsesPublic {
    fileprivate subscript(index: PublicImportType) -> PublicImportType {
        fatalError()
    }
}
fileprivate struct FileprivateSubscriptUsesPackage {
    fileprivate subscript(index: PackageImportType) -> PackageImportType {
        fatalError()
    }
}
fileprivate struct FileprivateSubscriptUsesInternal {
    fileprivate subscript(index: InternalImportType) -> InternalImportType {
        fatalError()
    }
}
fileprivate struct FileprivateSubscriptUsesFileprivate {
    fileprivate subscript(index: FileprivateImportType) -> FileprivateImportType {
        fatalError()
    }
}
fileprivate struct FileprivateSubscriptUsesPrivate {
    fileprivate subscript(index: PrivateImportType) -> PrivateImportType {
        fatalError()
    }
}

// Private subscripts
private struct PrivateSubscriptUsesPublic {
    private subscript(index: PublicImportType) -> PublicImportType {
        fatalError()
    }
}
private struct PrivateSubscriptUsesPackage {
    private subscript(index: PackageImportType) -> PackageImportType {
        fatalError()
    }
}
private struct PrivateSubscriptUsesInternal {
    private subscript(index: InternalImportType) -> InternalImportType {
        fatalError()
    }
}
private struct PrivateSubscriptUsesFileprivate {
    private subscript(index: FileprivateImportType) -> FileprivateImportType {
        fatalError()
    }
}
private struct PrivateSubscriptUsesPrivate {
    private subscript(index: PrivateImportType) -> PrivateImportType {
        fatalError()
    }
}

public protocol PublicProtoUsesPublic: PublicImportProto where T == PublicImportType {}
public protocol PublicProtoWherePackage: PublicImportProto where T == PackageImportType {} // expected-error {{public protocol's 'where' clause cannot use a package struct}}
// expected-note @-1 {{is imported by this file as}}
public protocol PublicProtoWhereInternal: PublicImportProto where T == InternalImportType {} // expected-error {{public protocol's 'where' clause cannot use an internal struct}}
// expected-note @-1 {{is imported by this file as}}
public protocol PublicProtoWhereFileprivate: PublicImportProto where T == FileprivateImportType {} // expected-error {{public protocol's 'where' clause cannot use a fileprivate struct}}
// expected-note @-1 {{is imported by this file as}}
public protocol PublicProtoWherePrivate: PublicImportProto where T == PrivateImportType {} // expected-error {{public protocol's 'where' clause cannot use a private struct}}
// expected-note @-1 {{is imported by this file as}}

public protocol PublicProtoRefinesPublic: PublicImportProto {}
public protocol PublicProtoRefinesPackage: PackageImportProto {} // expected-error {{public protocol cannot refine a package protocol}}
// expected-note @-1 {{is imported by this file as}}
public protocol PublicProtoRefinesInternal: InternalImportProto {} // expected-error {{public protocol cannot refine an internal protocol}}
// expected-note @-1 {{is imported by this file as}}
public protocol PublicProtoRefinesFileprivate: FileprivateImportProto {} // expected-error {{public protocol cannot refine a fileprivate protocol}}
// expected-note @-1 {{is imported by this file as}}
public protocol PublicProtoRefinesPrivate: PrivateImportProto {} // expected-error {{public protocol cannot refine a private protocol}}
// expected-note @-1 {{is imported by this file as}}

public class PublicSubclassPublic : PublicImportClass {}
public class PublicSubclassPackage : PackageImportClass {} // expected-error {{class cannot be declared public because its superclass is package}}
// expected-note @-1 {{is imported by this file as}}
public class PublicSubclassInternal : InternalImportClass {} // expected-error {{class cannot be declared public because its superclass is internal}}
// expected-note @-1 {{is imported by this file as}}
public class PublicSubclassFileprivate : FileprivateImportClass {} // expected-error {{class cannot be declared public because its superclass is fileprivate}}
// expected-note @-1 {{is imported by this file as}}
public class PublicSubclassPrivate : PrivateImportClass {} // expected-error {{class cannot be declared public because its superclass is private}}
// expected-note @-1 {{is imported by this file as}}


package protocol PackageProtoUsesPublic: PublicImportProto where T == PublicImportType {}
package protocol PackageProtoWherePackage: PublicImportProto where T == PackageImportType {}
package protocol PackageProtoWhereInternal: PublicImportProto where T == InternalImportType {} // expected-error {{package protocol's 'where' clause cannot use an internal struct}}
// expected-note @-1 {{is imported by this file as}}
package protocol PackageProtoWhereFileprivate: PublicImportProto where T == FileprivateImportType {} // expected-error {{package protocol's 'where' clause cannot use a fileprivate struct}}
// expected-note @-1 {{is imported by this file as}}
package protocol PackageProtoWherePrivate: PublicImportProto where T == PrivateImportType {} // expected-error {{package protocol's 'where' clause cannot use a private struct}}
// expected-note @-1 {{is imported by this file as}}

package protocol PackageProtoRefinesPublic: PublicImportProto {}
package protocol PackageProtoRefinesPackage: PackageImportProto {}
package protocol PackageProtoRefinesInternal: InternalImportProto {} // expected-error {{package protocol cannot refine an internal protocol}}
// expected-note @-1 {{is imported by this file as}}
package protocol PackageProtoRefinesFileprivate: FileprivateImportProto {} // expected-error {{package protocol cannot refine a fileprivate protocol}}
// expected-note @-1 {{is imported by this file as}}
package protocol PackageProtoRefinesPrivate: PrivateImportProto {} // expected-error {{package protocol cannot refine a private protocol}}
// expected-note @-1 {{is imported by this file as}}

package class PackageSubclassPublic : PublicImportClass {}
package class PackageSubclassPackage : PackageImportClass {}
package class PackageSubclassInternal : InternalImportClass {} // expected-error {{class cannot be declared package because its superclass is internal}}
// expected-note @-1 {{is imported by this file as}}
package class PackageSubclassFileprivate : FileprivateImportClass {} // expected-error {{class cannot be declared package because its superclass is fileprivate}}
// expected-note @-1 {{is imported by this file as}}
package class PackageSubclassPrivate : PrivateImportClass {} // expected-error {{class cannot be declared package because its superclass is private}}
// expected-note @-1 {{is imported by this file as}}


internal protocol InternalProtoUsesPublic: PublicImportProto where T == PublicImportType {}
internal protocol InternalProtoWherePackage: PublicImportProto where T == PackageImportType {}
internal protocol InternalProtoWhereInternal: PublicImportProto where T == InternalImportType {}
internal protocol InternalProtoWhereFileprivate: PublicImportProto where T == FileprivateImportType {} // expected-error {{internal protocol's 'where' clause cannot use a fileprivate struct}}
// expected-note @-1 {{is imported by this file as}}
internal protocol InternalProtoWherePrivate: PublicImportProto where T == PrivateImportType {} // expected-error {{internal protocol's 'where' clause cannot use a private struct}}
// expected-note @-1 {{is imported by this file as}}

internal protocol InternalProtoRefinesPublic: PublicImportProto {}
internal protocol InternalProtoRefinesPackage: PackageImportProto {}
internal protocol InternalProtoRefinesInternal: InternalImportProto {}
internal protocol InternalProtoRefinesFileprivate: FileprivateImportProto {} // expected-error {{internal protocol cannot refine a fileprivate protocol}}
// expected-note @-1 {{is imported by this file as}}
internal protocol InternalProtoRefinesPrivate: PrivateImportProto {} // expected-error {{internal protocol cannot refine a private protocol}}
// expected-note @-1 {{is imported by this file as}}

internal class InternalSubclassPublic : PublicImportClass {}
internal class InternalSubclassPackage : PackageImportClass {}
internal class InternalSubclassInternal : InternalImportClass {}
internal class InternalSubclassFileprivate : FileprivateImportClass {} // expected-error {{class cannot be declared internal because its superclass is fileprivate}}
// expected-note @-1 {{is imported by this file as}}
internal class InternalSubclassPrivate : PrivateImportClass {} // expected-error {{class cannot be declared internal because its superclass is private}}
// expected-note @-1 {{is imported by this file as}}


fileprivate protocol FileprivateProtoUsesPublic: PublicImportProto where T == PublicImportType {}
fileprivate protocol FileprivateProtoWherePackage: PublicImportProto where T == PackageImportType {}
fileprivate protocol FileprivateProtoWhereInternal: PublicImportProto where T == InternalImportType {}
fileprivate protocol FileprivateProtoWhereFileprivate: PublicImportProto where T == FileprivateImportType {}
fileprivate protocol FileprivateProtoWherePrivate: PublicImportProto where T == PrivateImportType {}

fileprivate protocol FileprivateProtoRefinesPublic: PublicImportProto {}
fileprivate protocol FileprivateProtoRefinesPackage: PackageImportProto {}
fileprivate protocol FileprivateProtoRefinesInternal: InternalImportProto {}
fileprivate protocol FileprivateProtoRefinesFileprivate: FileprivateImportProto {}
fileprivate protocol FileprivateProtoRefinesPrivate: PrivateImportProto {}

fileprivate class FileprivateSubclassPublic : PublicImportClass {}
fileprivate class FileprivateSubclassPackage : PackageImportClass {}
fileprivate class FileprivateSubclassInternal : InternalImportClass {}
fileprivate class FileprivateSubclassFileprivate : FileprivateImportClass {}
fileprivate class FileprivateSubclassPrivate : PrivateImportClass {}


private protocol PrivateProtoUsesPublic: PublicImportProto where T == PublicImportType {}
private protocol PrivateProtoWherePackage: PublicImportProto where T == PackageImportType {}
private protocol PrivateProtoWhereInternal: PublicImportProto where T == InternalImportType {}
private protocol PrivateProtoWhereFileprivate: PublicImportProto where T == FileprivateImportType {}
private protocol PrivateProtoWherePrivate: PublicImportProto where T == PrivateImportType {}

private protocol PrivateProtoRefinesPublic: PublicImportProto {}
private protocol PrivateProtoRefinesPackage: PackageImportProto {}
private protocol PrivateProtoRefinesInternal: InternalImportProto {}
private protocol PrivateProtoRefinesFileprivate: FileprivateImportProto {}
private protocol PrivateProtoRefinesPrivate: PrivateImportProto {}

private class PrivateSubclassPublic : PublicImportClass {}
private class PrivateSubclassPackage : PackageImportClass {}
private class PrivateSubclassInternal : InternalImportClass {}
private class PrivateSubclassFileprivate : FileprivateImportClass {}
private class PrivateSubclassPrivate : PrivateImportClass {}

public struct PublicTypeAliasUses {
    public typealias TAPublic = PublicImportProto
    public typealias TAPackage = PackageImportProto // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
// expected-note @-1 {{is imported by this file as}}
    public typealias TAInternal = InternalImportProto // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    public typealias TAFileprivate = FileprivateImportProto // expected-error {{type alias cannot be declared public because its underlying type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    public typealias TAPrivate = PrivateImportProto // expected-error {{type alias cannot be declared public because its underlying type uses a private type}}
// expected-note @-1 {{is imported by this file as}}
}

package struct PackageTypeAliasUses {
    package typealias TAPublic = PublicImportProto
    package typealias TAPackage = PackageImportProto
    package typealias TAInternal = InternalImportProto // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    package typealias TAFileprivate = FileprivateImportProto // expected-error {{type alias cannot be declared package because its underlying type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    package typealias TAPrivate = PrivateImportProto // expected-error {{type alias cannot be declared package because its underlying type uses a private type}}
// expected-note @-1 {{is imported by this file as}}
}

internal struct InternalTypeAliasUses {
    internal typealias TAPublic = PublicImportProto
    internal typealias TAPackage = PackageImportProto
    internal typealias TAInternal = InternalImportProto
    internal typealias TAFileprivate = FileprivateImportProto // expected-error {{type alias cannot be declared internal because its underlying type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    internal typealias TAPrivate = PrivateImportProto // expected-error {{type alias cannot be declared internal because its underlying type uses a private type}}
// expected-note @-1 {{is imported by this file as}}
}

fileprivate struct FileprivateTypeAliasUses {
    fileprivate typealias TAPublic = PublicImportProto
    fileprivate typealias TAPackage = PackageImportProto
    fileprivate typealias TAInternal = InternalImportProto
    fileprivate typealias TAFileprivate = FileprivateImportProto
    fileprivate typealias TAPrivate = PrivateImportProto
}

private struct PrivateTypeAliasUses {
    private typealias TAPublic = PublicImportProto
    private typealias TAPackage = PackageImportProto
    private typealias TAInternal = InternalImportProto
    private typealias TAFileprivate = FileprivateImportProto
    private typealias TAPrivate = PrivateImportProto
}

public protocol PublicProtocol {
    associatedtype ATDefaultPublic = PublicImportProto
    associatedtype ATDefaultPackage = PackageImportProto // expected-error {{associated type in a public protocol uses a package type in its default definition}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATDefaultInternal = InternalImportProto // expected-error {{associated type in a public protocol uses an internal type in its default definition}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATDefaultFileprivate = FileprivateImportProto // expected-error {{associated type in a public protocol uses a fileprivate type in its default definition}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATDefaultPrivate = PrivateImportProto // expected-error {{associated type in a public protocol uses a private type in its default definition}}
// expected-note @-1 {{is imported by this file as}}

    associatedtype ATRequirePublic: PublicImportProto
    associatedtype ATRequirePackage: PackageImportProto // expected-error {{associated type in a public protocol uses a package type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATRequireInternal: InternalImportProto // expected-error {{associated type in a public protocol uses an internal type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATRequireFileprivate: FileprivateImportProto // expected-error {{associated type in a public protocol uses a fileprivate type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATRequirePrivate: PrivateImportProto // expected-error {{associated type in a public protocol uses a private type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
}

package protocol PackageProtocol {
    associatedtype ATDefaultPublic = PublicImportProto
    associatedtype ATDefaultPackage = PackageImportProto
    associatedtype ATDefaultInternal = InternalImportProto // expected-error {{associated type in a package protocol uses an internal type in its default definition}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATDefaultFileprivate = FileprivateImportProto // expected-error {{associated type in a package protocol uses a fileprivate type in its default definition}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATDefaultPrivate = PrivateImportProto // expected-error {{associated type in a package protocol uses a private type in its default definition}}
// expected-note @-1 {{is imported by this file as}}

    associatedtype ATRequirePublic: PublicImportProto
    associatedtype ATRequirePackage: PackageImportProto
    associatedtype ATRequireInternal: InternalImportProto // expected-error {{associated type in a package protocol uses an internal type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATRequireFileprivate: FileprivateImportProto // expected-error {{associated type in a package protocol uses a fileprivate type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATRequirePrivate: PrivateImportProto // expected-error {{associated type in a package protocol uses a private type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
}

internal protocol InternalProtocol {
    associatedtype ATDefaultPublic = PublicImportProto
    associatedtype ATDefaultPackage = PackageImportProto
    associatedtype ATDefaultInternal = InternalImportProto
    associatedtype ATDefaultFileprivate = FileprivateImportProto // expected-error {{associated type in an internal protocol uses a fileprivate type in its default definition}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATDefaultPrivate = PrivateImportProto // expected-error {{associated type in an internal protocol uses a private type in its default definition}}
// expected-note @-1 {{is imported by this file as}}

    associatedtype ATRequirePublic: PublicImportProto
    associatedtype ATRequirePackage: PackageImportProto
    associatedtype ATRequireInternal: InternalImportProto
    associatedtype ATRequireFileprivate: FileprivateImportProto // expected-error {{associated type in an internal protocol uses a fileprivate type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
    associatedtype ATRequirePrivate: PrivateImportProto // expected-error {{associated type in an internal protocol uses a private type in its requirement}}
// expected-note @-1 {{is imported by this file as}}
}

fileprivate protocol FileprivateProtocol {
    associatedtype ATDefaultPublic = PublicImportProto
    associatedtype ATDefaultPackage = PackageImportProto
    associatedtype ATDefaultInternal = InternalImportProto
    associatedtype ATDefaultFileprivate = FileprivateImportProto
    // Accocciated type have a minimum formal access level at internal.
    associatedtype ATDefaultPrivate = PrivateImportProto

    associatedtype ATRequirePublic: PublicImportProto
    associatedtype ATRequirePackage: PackageImportProto
    associatedtype ATRequireInternal: InternalImportProto
    associatedtype ATRequireFileprivate: FileprivateImportProto
    associatedtype ATRequirePrivate: PrivateImportProto
}

private protocol PrivateProtocol {
    associatedtype ATDefaultPublic = PublicImportProto
    associatedtype ATDefaultPackage = PackageImportProto
    associatedtype ATDefaultInternal = InternalImportProto
    associatedtype ATDefaultFileprivate = FileprivateImportProto
    associatedtype ATDefaultPrivate = PrivateImportProto

    associatedtype ATRequirePublic: PublicImportProto
    associatedtype ATRequirePackage: PackageImportProto
    associatedtype ATRequireInternal: InternalImportProto
    associatedtype ATRequireFileprivate: FileprivateImportProto
    associatedtype ATRequirePrivate: PrivateImportProto
}

public struct PublicVars {
    public var a: PublicImportType
    public var b: PackageImportType // expected-error {{property cannot be declared public because its type uses a package type}}
// expected-note @-1 {{is imported by this file as}}
    public var c: InternalImportType // expected-error {{property cannot be declared public because its type uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    public var d: FileprivateImportType // expected-error {{property cannot be declared public because its type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    public var e: PrivateImportType // expected-error {{property cannot be declared public because its type uses a private type}}
// expected-note @-1 {{is imported by this file as}}

    @PublicLibWrapper
    public var f: PublicImportType
    @PackageLibWrapper
    public var g: PublicImportType // expected-error {{property cannot be declared public because its property wrapper type uses a package type}}
// expected-note @-1 {{is imported by this file as}}
    @InternalLibWrapper
    public var h: PublicImportType // expected-error {{property cannot be declared public because its property wrapper type uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    @FileprivateLibWrapper
    public var i: PublicImportType // expected-error {{property cannot be declared public because its property wrapper type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    @PrivateLibWrapper
    public var j: PublicImportType // expected-error {{property cannot be declared public because its property wrapper type uses a private type}}
// expected-note @-1 {{is imported by this file as}}

    public var k = PublicImportType()
    public var l = PackageImportType() // expected-error {{property cannot be declared public because its type 'PackageImportType' uses a package type}}
// expected-note @-1 {{is imported by this file as}}
    public var m = InternalImportType() // expected-error {{property cannot be declared public because its type 'InternalImportType' uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    public var n = FileprivateImportType() // expected-error {{property cannot be declared public because its type 'FileprivateImportType' uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    public var o = PrivateImportType() // expected-error {{property cannot be declared public because its type 'PrivateImportType' uses a private type}}
// expected-note @-1 {{is imported by this file as}}
}

package struct PackageVars {
    package var a: PublicImportType
    package var b: PackageImportType
    package var c: InternalImportType // expected-error {{property cannot be declared package because its type uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    package var d: FileprivateImportType // expected-error {{property cannot be declared package because its type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    package var e: PrivateImportType // expected-error {{property cannot be declared package because its type uses a private type}}
// expected-note @-1 {{is imported by this file as}}

    @PublicLibWrapper
    package var f: PublicImportType
    @PackageLibWrapper
    package var g: PublicImportType
    @InternalLibWrapper
    package var h: PublicImportType // expected-error {{property cannot be declared package because its property wrapper type uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    @FileprivateLibWrapper
    package var i: PublicImportType // expected-error {{property cannot be declared package because its property wrapper type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    @PrivateLibWrapper
    package var j: PublicImportType // expected-error {{property cannot be declared package because its property wrapper type uses a private type}}
// expected-note @-1 {{is imported by this file as}}

    package var k = PublicImportType()
    package var l = PackageImportType()
    package var m = InternalImportType() // expected-error {{property cannot be declared package because its type 'InternalImportType' uses an internal type}}
// expected-note @-1 {{is imported by this file as}}
    package var n = FileprivateImportType() // expected-error {{property cannot be declared package because its type 'FileprivateImportType' uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    package var o = PrivateImportType() // expected-error {{property cannot be declared package because its type 'PrivateImportType' uses a private type}}
// expected-note @-1 {{is imported by this file as}}
}

internal struct InternalVars {
    internal var a: PublicImportType
    internal var b: PackageImportType
    internal var c: InternalImportType
    internal var d: FileprivateImportType // expected-error {{property cannot be declared internal because its type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    internal var e: PrivateImportType // expected-error {{property cannot be declared internal because its type uses a private type}}
// expected-note @-1 {{is imported by this file as}}

    @PublicLibWrapper
    internal var f: PublicImportType
    @PackageLibWrapper
    internal var g: PublicImportType
    @InternalLibWrapper
    internal var h: PublicImportType
    @FileprivateLibWrapper
    internal var i: PublicImportType // expected-error {{property cannot be declared internal because its property wrapper type uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    @PrivateLibWrapper
    internal var j: PublicImportType // expected-error {{property cannot be declared internal because its property wrapper type uses a private type}}
// expected-note @-1 {{is imported by this file as}}

    internal var k = PublicImportType()
    internal var l = PackageImportType()
    internal var m = InternalImportType()
    internal var n = FileprivateImportType() // expected-error {{property cannot be declared internal because its type 'FileprivateImportType' uses a fileprivate type}}
// expected-note @-1 {{is imported by this file as}}
    internal var o = PrivateImportType() // expected-error {{property cannot be declared internal because its type 'PrivateImportType' uses a private type}}
// expected-note @-1 {{is imported by this file as}}
}

fileprivate struct FileprivateVars {
    fileprivate var a: PublicImportType
    fileprivate var b: PackageImportType
    fileprivate var c: InternalImportType
    fileprivate var d: FileprivateImportType
    fileprivate var e: PrivateImportType

    @PublicLibWrapper
    fileprivate var f: PublicImportType
    @PackageLibWrapper
    fileprivate var g: PublicImportType
    @InternalLibWrapper
    fileprivate var h: PublicImportType
    @FileprivateLibWrapper
    fileprivate var i: PublicImportType
    @PrivateLibWrapper
    fileprivate var j: PublicImportType

    fileprivate var k = PublicImportType()
    fileprivate var l = PackageImportType()
    fileprivate var m = InternalImportType()
    fileprivate var n = FileprivateImportType()
    fileprivate var o = PrivateImportType()
}

private struct PrivateVars {
    private var a: PublicImportType
    private var b: PackageImportType
    private var c: InternalImportType
    private var d: FileprivateImportType
    private var e: PrivateImportType

    @PublicLibWrapper
    private var f: PublicImportType
    @PackageLibWrapper
    private var g: PublicImportType
    @InternalLibWrapper
    private var h: PublicImportType
    @FileprivateLibWrapper
    private var i: PublicImportType
    @PrivateLibWrapper
    private var j: PublicImportType

    private var k = PublicImportType()
    private var l = PackageImportType()
    private var m = InternalImportType()
    private var n = FileprivateImportType()
    private var o = PrivateImportType()
}

public struct PublicGenericUsesPublic<A: PublicImportProto> {}
public struct PublicGenericUsesPackage<A: PackageImportProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a package type}}
public struct PublicGenericUsesInternal<A: InternalImportProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses an internal type}}
public struct PublicGenericUsesFileprivate<A: FileprivateImportProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a fileprivate type}}
public struct PublicGenericUsesPrivate<A: PrivateImportProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a private type}}

package struct PackageGenericUsesPublic<A: PublicImportProto> {}
package struct PackageGenericUsesPackage<A: PackageImportProto> {}
package struct PackageGenericUsesInternal<A: InternalImportProto> {} // expected-error {{generic struct cannot be declared package because its generic parameter uses an internal type}}
package struct PackageGenericUsesFileprivate<A: FileprivateImportProto> {} // expected-error {{generic struct cannot be declared package because its generic parameter uses a fileprivate type}}
package struct PackageGenericUsesPrivate<A: PrivateImportProto> {} // expected-error {{generic struct cannot be declared package because its generic parameter uses a private type}}

internal struct InternalGenericUsesPublic<A: PublicImportProto> {}
internal struct InternalGenericUsesPackage<A: PackageImportProto> {}
internal struct InternalGenericUsesInternal<A: InternalImportProto> {}
internal struct InternalGenericUsesFileprivate<A: FileprivateImportProto> {} // expected-error {{generic struct cannot be declared internal because its generic parameter uses a fileprivate type}}
internal struct InternalGenericUsesPrivate<A: PrivateImportProto> {} // expected-error {{generic struct cannot be declared internal because its generic parameter uses a private type}}

fileprivate struct FileprivateGenericUsesPublic<A: PublicImportProto> {}
fileprivate struct FileprivateGenericUsesPackage<A: PackageImportProto> {}
fileprivate struct FileprivateGenericUsesInternal<A: InternalImportProto> {}
fileprivate struct FileprivateGenericUsesFileprivate<A: FileprivateImportProto> {}
fileprivate struct FileprivateGenericUsesPrivate<A: PrivateImportProto> {}

private struct PrivateGenericUsesPublic<A: PublicImportProto> {}
private struct PrivateGenericUsesPackage<A: PackageImportProto> {}
private struct PrivateGenericUsesInternal<A: InternalImportProto> {}
private struct PrivateGenericUsesFileprivate<A: FileprivateImportProto> {}
private struct PrivateGenericUsesPrivate<A: PrivateImportProto> {}

public struct PublicGenericUsesProtocolComposition<A: FileprivateImportProto & InternalImportProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a fileprivate type}}
