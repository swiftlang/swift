// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -I %t

import COM

// Missing requirements should not produce follow-on conformance diagnostics.
// expected-error@+1 {{'COMInterface' conformance is compiler-managed and cannot be declared explicitly}}
final class ExplicitCOMInterface: COMInterface {}

// expected-error@+1 {{'COMActivatable' conformance is compiler-managed and cannot be declared explicitly}}
final class ExplicitCOMActivatable: COMActivatable {}

// Supplying a witness does not make a source conformance valid.
// expected-error@+1 {{'COMInterface' conformance is compiler-managed and cannot be declared explicitly}}
struct WitnessedInterface: COMInterface {
  var IID: IID { fatalError() }
}

// expected-error@+1 {{'COMActivatable' conformance is compiler-managed and cannot be declared explicitly}}
struct WitnessedActivatable: COMActivatable {
  var CLSID: CLSID { fatalError() }
}

// expected-error@+1 {{'COMInterface' conformance is compiler-managed and cannot be declared explicitly}}
protocol RefinesInterface: COMInterface {}
// expected-error@+1 {{'COMActivatable' conformance is compiler-managed and cannot be declared explicitly}}
protocol RefinesActivatable: COMActivatable {}

struct InterfaceExtension {}
// expected-error@+1 {{'COMInterface' conformance is compiler-managed and cannot be declared explicitly}}
extension InterfaceExtension: COMInterface {}

struct ActivatableExtension {}
// expected-error@+1 {{'COMActivatable' conformance is compiler-managed and cannot be declared explicitly}}
extension ActivatableExtension: COMActivatable {}

// expected-error@+1 {{cannot extend protocol 'COMInterface'}}
extension COMInterface {
  var member: Int { 0 }
}

// The activation protocol can have convenience members in client modules.
extension COMActivatable {
  var member: Int { 0 }
}

// expected-error@+1 {{'any COMInterface' is invalid because 'COMInterface' describes a COM metatype identity}}
func rejectInterface(_: any COMInterface) {}
// expected-error@+1 {{'any COMActivatable' is invalid because 'COMActivatable' describes a COM metatype identity}}
func rejectActivatable(_: any COMActivatable) {}

// Aggregation remains an ordinary class-bound, explicitly declared conformance.
final class Aggregated: COMAggregatable {
  var controller: (any IUnknown)? { nil }
}

// Compositions cannot hide a refinement or an identity existential.
// expected-error@+1 {{'COMInterface' conformance is compiler-managed and cannot be declared explicitly}}
protocol RefinesInterfaceComposition: COMInterface & Sendable {}
// expected-error@+1 {{'COMActivatable' conformance is compiler-managed and cannot be declared explicitly}}
protocol RefinesActivatableComposition: COMActivatable & Sendable {}

// expected-error@+1 {{'any COMInterface' is invalid because 'COMInterface' describes a COM metatype identity}}
func rejectInterfaceComposition(_: any COMInterface & Sendable) {}
// expected-error@+1 {{'any COMActivatable' is invalid because 'COMActivatable' describes a COM metatype identity}}
func rejectActivatableComposition(_: any COMActivatable & Sendable) {}
