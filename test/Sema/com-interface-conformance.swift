// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -I %t

final class ExplicitCOMInterface: COMInterface { }
// expected-error@-1 {{'COMInterface' conformance is compiler-managed and cannot be declared explicitly}}

protocol ExplicitCOMProtocol: COMInterface { }
// expected-error@-1 {{'COMInterface' conformance is compiler-managed and cannot be declared explicitly}}

extension COMInterface { // expected-error {{cannot extend protocol 'COMInterface'}}
  var clientDefinedMember: Int { 0 }
}

#if $_MicrosoftCOM

// expected-error@+1{{'COMActivatable' conformance is compiler-managed and cannot be declared explicitly}}
final class ExplicitCOMActivatable: COMActivatable {
  var CLSID: CLSID { fatalError() }
}

// expected-error@+1{{'any COMActivatable' is invalid because 'COMActivatable' describes a COM metatype identity}}
func reject(_: any COMActivatable) {
}

#endif
