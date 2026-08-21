// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=corefoundation %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=corefoundation -I %t

import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IRootless {}

@com(implementation: "20000000-0000-0000-0000-000000000001")
final class Implementation: IRootless {}

let _: IID = IRootless.IID
let _ = Implementation.CLSID
// expected-error@-1 {{type 'Implementation' has no member 'CLSID'}}

func requiresSwiftIdentity<T: ISwiftObject>(_: T) {}
// expected-note@+1 {{where 'T' = 'Implementation'}}
func requiresMicrosoftRoot<T: IUnknown>(_: T) {}

func check(_ value: Implementation) {
  requiresSwiftIdentity(value)
  requiresMicrosoftRoot(value)
  // expected-error@-1 {{global function 'requiresMicrosoftRoot' requires that 'Implementation' conform to 'IUnknown'}}
}
