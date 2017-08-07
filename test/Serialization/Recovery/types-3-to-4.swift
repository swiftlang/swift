// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -swift-version 3 %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -swift-version 3 | %FileCheck -check-prefix=CHECK-3 %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -swift-version 4 | %FileCheck -check-prefix=CHECK-4 %s

// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/custom-modules  -swift-version 4 -D TEST -verify

// REQUIRES: objc_interop

#if TEST

import Lib

func requiresConformance(_: B_RequiresConformance<B_ConformsToProto>) {}
func requiresConformance(_: B_RequiresConformance<C_RelyOnConformanceImpl.Assoc>) {}

class Sub: Base {} // okay
class Impl: Proto {} // expected-error {{type 'Impl' does not conform to protocol 'Proto'}}

#else // TEST

import Types

// Please use prefixes to keep the printed parts of this file in alphabetical
// order.

public func A_renameAllTheThings(
  a: Swift3RenamedClass?,
  b: Swift3RenamedGenericClass<AnyObject>?,
  c: Swift3RenamedTypedef,
  d: Swift3RenamedStruct,
  e: Swift3RenamedEnum,
  f: Swift3RenamedProtocol
) {}

// CHECK-4-LABEL: func A_renameAllTheThings(
// CHECK-4-SAME: a: RenamedClass?
// CHECK-4-SAME: b: RenamedGenericClass<AnyObject>?
// CHECK-4-SAME: c: RenamedTypedef
// CHECK-4-SAME: d: RenamedStruct
// CHECK-4-SAME: e: RenamedEnum
// CHECK-4-SAME: f: RenamedProtocol
// CHECK-4-SAME: )


// CHECK-3-LABEL: func A_renameAllTheThings(
// CHECK-3-SAME: a: Swift3RenamedClass?

// FIXME: An issue not specific to the importer where generic typealiases are
// not preserved when provided arguments.
// CHECK-3-SAME: b: RenamedGenericClass<AnyObject>?

// CHECK-3-SAME: c: Swift3RenamedTypedef
// CHECK-3-SAME: d: Swift3RenamedStruct
// CHECK-3-SAME: e: Swift3RenamedEnum
// CHECK-3-SAME: f: Swift3RenamedProtocol
// CHECK-3-SAME: )


public func A_renameGeneric<T: Swift3RenamedProtocol>(obj: T) {}

// CHECK-4-LABEL: func A_renameGeneric<T>(
// CHECK-4-SAME: where T : RenamedProtocol

// FIXME: Preserve sugar in requirements.
// CHECK-3-LABEL: func A_renameGeneric<T>(
// CHECK-3-SAME: where T : RenamedProtocol

public class B_ConformsToProto: Swift3RenamedProtocol {}

// CHECK-4-LABEL: class B_ConformsToProto : RenamedProtocol
// CHECK-3-LABEL: class B_ConformsToProto : Swift3RenamedProtocol

public struct B_RequiresConformance<T: Swift3RenamedProtocol> {}

// CHECK-4-LABEL: struct B_RequiresConformance<T> where T : RenamedProtocol

// FIXME: Preserve sugar in requirements.
// CHECK-3-LABEL: struct B_RequiresConformance<T> where T : RenamedProtocol

public protocol C_RelyOnConformance {
  associatedtype Assoc: Swift3RenamedProtocol
}

public class C_RelyOnConformanceImpl: C_RelyOnConformance {
  public typealias Assoc = Swift3RenamedClass
}

open class Base {
  public init(wrapped: NewlyWrappedTypedef) {}
}
public protocol Proto {
  func useWrapped(_ wrapped: NewlyWrappedTypedef)
}

#endif
