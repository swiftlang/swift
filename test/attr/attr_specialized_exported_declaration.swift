// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -O -enable-default-cmo -enable-experimental-feature ExtendedSpecializedAttribute -emit-module -emit-module-path %t/Library.swiftmodule
// RUN: %target-swift-frontend %t/Exporter.swift -I %t -parse-as-library -enable-experimental-feature ExtendedSpecializedAttribute -emit-module -emit-module-path %t/Exporter.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t -parse-as-library -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-swift-frontend -typecheck -verify %t/NoFeature.swift -I %t -parse-as-library

// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- Library.swift

public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b }
public func ambiguous<T: FixedWidthInteger>(_ a: T) -> T { a }
@_transparent public func transparent<T: FixedWidthInteger>(_ a: T) -> T { a }
@specialized(exported: true, where T == Int)
public func exported<T: FixedWidthInteger>(_ a: T) -> T { a }
public func exportedByOther<T: FixedWidthInteger>(_ a: T) -> T { a }
public struct Box<T> {
  public var v: T
  public func get() -> T { v }
}
@specialized(exported: true, where T == Int)
public struct Exported<T> {}
public struct NonGeneric {}
public protocol Proto {}

//--- Exporter.swift

import Library

@specialized(exported: true, where T == Int)
public func exportedByOther<T: FixedWidthInteger>(_ a: T) -> T

public func ambiguous<T: FixedWidthInteger>(_ a: T) -> T { a }

//--- Client.swift

import Library
import Exporter

@specialized(exported: true, where T == Int)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T // expected-note {{'sum' declared here}}

// It doesn't declare a function, so calls still go to 'Library.sum'.
func use(_ x: Int) -> Int { sum(x, x) }

@specialized(exported: true, where T == UInt8) // expected-error {{'@specialized' attributes for 'sum' belong on one declaration without a body}}
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

@specialized(exported: true, where T == Int) // expected-error {{'@specialized' declaration without a body must name exactly one function 'sum' of type '<T where T : FixedWidthInteger> (T) -> T' in an imported module}}
public func sum<T: FixedWidthInteger>(_ a: T) -> T

@specialized(exported: true, where T == Int) // expected-error {{'@specialized' declaration without a body must name exactly one function 'ambiguous' of type '<T where T : FixedWidthInteger> (T) -> T' in an imported module}}
public func ambiguous<T: FixedWidthInteger>(_ a: T) -> T

@specialized(exported: true, where T == Int) // expected-error {{'@specialized(exported: true)' cannot be combined with '@_transparent'}}
public func transparent<T: FixedWidthInteger>(_ a: T) -> T

@specialized(exported: true, where T == Int) // expected-error {{module 'Library' already exports this specialization of 'exported'}}
public func exported<T: FixedWidthInteger>(_ a: T) -> T

@specialized(exported: true, where T == Int) // expected-error {{module 'Exporter' already exports this specialization of 'exportedByOther'}}
public func exportedByOther<T: FixedWidthInteger>(_ a: T) -> T

public func local<T: FixedWidthInteger>(_ a: T) -> T { a } // expected-note {{'local' previously declared here}}

@specialized(exported: true, where T == Int) // expected-error {{'@specialized' on 'local' of this module belongs on its definition, not on a declaration without a body}}
public func local<T: FixedWidthInteger>(_ a: T) -> T // expected-error {{invalid redeclaration of 'local'}}

// Members, in an extension.
extension Box {
  @specialized(exported: true, where T == Int)
  // The extension below already exports it.
  @specialized(exported: true, where T == UInt8) // expected-error {{module 'Client' already exports this specialization of 'get()'}}
  public func get() -> T

  @specialized(exported: true, where T == Int) // expected-error {{'@specialized' declaration without a body must name exactly one function 'nope()' of type '<T> (Box<T>) -> () -> T' in an imported module}}
  public func nope() -> T
}

// It doesn't declare a member, so calls still go to 'Library.Box.get'.
func useBox(_ b: Box<Int>) -> Int { b.get() }


// Whole types, with an extension.
@specialized(exported: true, where T == UInt8)
extension Box {}

@specialized(exported: true, where T == Int) // expected-error {{module 'Library' already exports this specialization of 'Exported'}}
extension Exported {}

@specialized(exported: true, where T == Int8) // expected-error {{'@specialized' requires an extension without generic constraints of its own}}
extension Box where T: Equatable {}

@specialized(exported: true, where T == Int8) // expected-error {{an extension with '@specialized' can only contain '@specialized' declarations without a body}}
extension Box {
  public func extra() {}
}

@specialized(exported: true, where T == Int) // expected-error {{'@specialized' on an extension requires a generic struct, enum or class}}
extension NonGeneric {}

@specialized(exported: true, where T == Int) // expected-error {{'@specialized' on an extension requires a generic struct, enum or class}}
extension Proto {}

public protocol LocalProto {}

@specialized(exported: true, where T == Int16) // expected-error {{an extension with '@specialized' cannot declare conformances}}
extension Box: LocalProto {}

//--- NoFeature.swift

import Library

@specialized(where T == Int) // expected-error {{'@specialized' on a declaration without a body requires '-enable-experimental-feature ExtendedSpecializedAttribute'}}
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

@specialized(where T == Int) // expected-error {{'@specialized' on an extension requires '-enable-experimental-feature ExtendedSpecializedAttribute'}}
extension Box {}
