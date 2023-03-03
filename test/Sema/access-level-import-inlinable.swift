// Test use of decls restricted by an import access-level in inlinable code.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

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
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-experimental-feature AccessLevelOnImport -verify

//--- PublicLib.swift
public protocol PublicImportProto {
    associatedtype T
}
public struct PublicImportType {
    public init() {}
}
public func PublicFunc() {}

@propertyWrapper
public struct PublicImportWrapper<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

//--- PackageLib.swift
public struct PackageImportType {
    public init() {}
}

//--- InternalLib.swift
public protocol InternalImportProto {
    associatedtype T
}
public struct InternalImportType {
    public init() {}
}
public func InternalFunc() {}

//--- FileprivateLib.swift
public protocol FileprivateImportProto {
    associatedtype T
}

@propertyWrapper
public struct FileprivateImportWrapper<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

//--- PrivateLib.swift
public struct PrivateImportType {
    public init() {}
}

//--- Client.swift
public import PublicLib
package import PackageLib // expected-note 2 {{module 'PackageLib' imported as 'package' here}}
internal import InternalLib // expected-note 12 {{module 'InternalLib' imported as 'internal' here}}
fileprivate import FileprivateLib // expected-note 6 {{module 'FileprivateLib' imported as 'fileprivate' here}}
private import PrivateLib // expected-note 6 {{module 'PrivateLib' imported as 'private' here}}

public struct GenericType<T, U> {}

@inlinable public func inlinable() {

  PublicFunc()
  InternalFunc() // expected-error {{global function 'InternalFunc()' is internal and cannot be referenced from an '@inlinable' function}}

  let _: PublicImportType
  let _: InternalImportType // expected-error {{struct 'InternalImportType' is internal and cannot be referenced from an '@inlinable' function}}

  let _ = PublicImportType()
  let _ = PrivateImportType() // expected-error {{struct 'PrivateImportType' is private and cannot be referenced from an '@inlinable' function}}
  // expected-error @-1 {{initializer 'init()' is private and cannot be referenced from an '@inlinable' function}}

  let _: any PublicImportProto
  let _: any InternalImportProto // expected-error {{protocol 'InternalImportProto' is internal and cannot be referenced from an '@inlinable' function}}

  let _: any FileprivateImportProto & InternalImportProto // expected-error {{protocol 'FileprivateImportProto' is fileprivate and cannot be referenced from an '@inlinable' function}}
  // expected-error @-1 {{protocol 'InternalImportProto' is internal and cannot be referenced from an '@inlinable' function}}

  func PublicFuncUsesPublic(_: PublicImportType) {}
  func PublicFuncUsesPackage(_: PackageImportType) {} // expected-error {{struct 'PackageImportType' is package and cannot be referenced from an '@inlinable' function}}}

  func PublicFuncUsesPublic() -> PublicImportType {
    fatalError()
  }
  func PublicFuncReturnUsesInternal() -> InternalImportType { // expected-error {{struct 'InternalImportType' is internal and cannot be referenced from an '@inlinable' function}}
    fatalError()
  }

  @PublicImportWrapper
  var wrappedPublic: PublicImportType

  @FileprivateImportWrapper // expected-error {{initializer 'init(wrappedValue:)' is fileprivate and cannot be referenced from an '@inlinable' function}}
  // expected-error @-1 {{generic struct 'FileprivateImportWrapper' is fileprivate and cannot be referenced from an '@inlinable' function}}
  var wrappedFileprivate: PublicImportType

  let _: GenericType<PublicImportType, PublicImportType>
  let _: GenericType<InternalImportType, PrivateImportType> // expected-error {{struct 'InternalImportType' is internal and cannot be referenced from an '@inlinable' function}}
  // expected-error @-1 {{struct 'PrivateImportType' is private and cannot be referenced from an '@inlinable' function}}
}

@_alwaysEmitIntoClient public func alwaysEmitIntoClient() {

  PublicFunc()
  InternalFunc() // expected-error {{global function 'InternalFunc()' is internal and cannot be referenced from an '@_alwaysEmitIntoClient' function}}

  let _: PublicImportType
  let _: InternalImportType // expected-error {{struct 'InternalImportType' is internal and cannot be referenced from an '@_alwaysEmitIntoClient' function}}

  let _ = PublicImportType()
  let _ = PrivateImportType() // expected-error {{struct 'PrivateImportType' is private and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
  // expected-error @-1 {{initializer 'init()' is private and cannot be referenced from an '@_alwaysEmitIntoClient' function}}

  let _: any PublicImportProto
  let _: any InternalImportProto // expected-error {{protocol 'InternalImportProto' is internal and cannot be referenced from an '@_alwaysEmitIntoClient' function}}

  let _: any FileprivateImportProto & InternalImportProto // expected-error {{protocol 'FileprivateImportProto' is fileprivate and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
  // expected-error @-1 {{protocol 'InternalImportProto' is internal and cannot be referenced from an '@_alwaysEmitIntoClient' function}}

  func PublicFuncUsesPublic(_: PublicImportType) {}
  func PublicFuncUsesPackage(_: PackageImportType) {} // expected-error {{struct 'PackageImportType' is package and cannot be referenced from an '@_alwaysEmitIntoClient' function}}}

  func PublicFuncUsesPublic() -> PublicImportType {
    fatalError()
  }
  func PublicFuncReturnUsesInternal() -> InternalImportType { // expected-error {{struct 'InternalImportType' is internal and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
    fatalError()
  }

  @PublicImportWrapper
  var wrappedPublic: PublicImportType

  @FileprivateImportWrapper // expected-error {{initializer 'init(wrappedValue:)' is fileprivate and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
  // expected-error @-1 {{generic struct 'FileprivateImportWrapper' is fileprivate and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
  var wrappedFileprivate: PublicImportType

  let _: GenericType<PublicImportType, PublicImportType>
  let _: GenericType<InternalImportType, PrivateImportType> // expected-error {{struct 'InternalImportType' is internal and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
  // expected-error @-1 {{struct 'PrivateImportType' is private and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
}
