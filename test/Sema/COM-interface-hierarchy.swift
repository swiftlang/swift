// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -emit-ir -o /dev/null -verify -enable-experimental-com-interop -I %t %s

import COM

@_marker
protocol BaseMarker {}

@_marker
protocol LocalMarker: BaseMarker {}

protocol SwiftProtocol {
  func requirement()
}

@com(interface: "A0000000-0000-0000-0000-000000000001")
protocol IBase {}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IMiddle: IBase {}

// Comparable COM bases form one ABI chain. Marker refinements do not
// contribute an ABI base.
@com(interface: "10000000-0000-0000-0000-000000000003")
protocol ILeaf: IMiddle, IBase, Sendable, LocalMarker {}

@com(interface: "10000000-0000-0000-0000-000000000004")
protocol IReverseLeaf: IBase, IMiddle {}

// AnyObject continues to carry the class-bound invariant until COM-reference
// existentials and conformance checking enforce it directly.
@com(interface: "20000000-0000-0000-0000-000000000001")
protocol IClassBound: AnyObject {}

@com(interface: "20000000-0000-0000-0000-000000000002")
protocol ISwiftRefinement: SwiftProtocol {}
// expected-error@-1 {{COM interface 'ISwiftRefinement' cannot inherit non-COM, non-marker protocol 'SwiftProtocol'}}

class NativeBase {}

@com(interface: "20000000-0000-0000-0000-000000000005")
protocol INativeBase: NativeBase {}
// expected-error@-1 {{COM interface 'INativeBase' cannot inherit non-protocol type 'NativeBase'}}

@com(interface: "20000000-0000-0000-0000-000000000003")
protocol IIndependent {}

@com(interface: "20000000-0000-0000-0000-000000000004")
protocol IMultipleBases: IBase, IIndependent {}
// expected-error@-1 {{COM interface 'IMultipleBases' cannot inherit unrelated COM interfaces 'IBase' and 'IIndependent'}}

protocol MissingIdentity: IBase {}
// expected-error@-1 {{protocol 'MissingIdentity' refines COM interface 'IBase' and must declare its own '@com(interface:)' identity}}

@com(interface: "A0000000-0000-0000-0000-000000000001")
protocol IRepeatedIID: IBase {}
// expected-error@-2 {{COM interface 'IRepeatedIID' and inherited interface 'IBase' use the same interface identifier 'A0000000-0000-0000-0000-000000000001'}}

@com(interface: "a0000000-0000-0000-0000-000000000001")
protocol IRepeatedIIDCaseInsensitive: IBase {}
// expected-error@-2 {{COM interface 'IRepeatedIIDCaseInsensitive' and inherited interface 'IBase' use the same interface identifier 'a0000000-0000-0000-0000-000000000001'}}
