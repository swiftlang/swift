// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

@com(interface: "00000000-0000-0000-0000-000000000000")
protocol IInterface1: IUnknown { }

@com(interface: "00000000-0000-0000-0000-000000000002")
protocol IInterface2: IInterface1 { }

@com(interface: "AAAAAAAA-AAAA-AAAA-AAAA-AAAAAAAAAAAA")
protocol IUpperCase: IUnknown { }

@com(interface: "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
protocol ILowerCase: IUnknown { }

@com
class CClass1 { }

@com
class CClass2: IUnknown { }

@com(implementation: "00000000-0000-0000-0000-000000000000")
class CClass3 { }

@com(implementation: "00000000-0000-0000-0000-000000000000")
class CClass4: IUnknown { }

@com(implementation: "00000000-0000-0000-0000-000000000000", threading: .apartment)
class CClass5: IUnknown { }

@com(implementation: "00000000-0000-0000-0000-000000000000")
class CClass6: IInterface1, IInterface2 { }

@com(implementation: "00000000-0000-0000-0000-000000000000")
final class CClass7 { }

@com(implementation: "00000000-0000-0000-0000-000000000000")
final class CClass8: IUnknown { }

@com(implementation: "00000000-0000-0000-0000-000000000000")
final class CClass9: IUnknown, IInterface1 { }

@com
protocol IInterface3: IUnknown { }
// expected-error@-2 {{'@com' on a protocol requires 'interface:' argument}}

@com(interface: "guid")
protocol IInterface4: IUnknown { }
// expected-error@-2 {{'guid' is not a valid GUID}}

@com(interface: "00000000-0000-0000-0000-00000000")
protocol IInterface5: IUnknown { }
// expected-error@-2 {{'00000000-0000-0000-0000-00000000' is not a valid GUID}}

@com(interface: "{00000000-0000-0000-0000-000000000000}")
protocol IInterface6: IUnknown { }
// expected-error@-2 {{'{00000000-0000-0000-0000-000000000000}' is not a valid GUID}}

@com(implementation: "00000000-0000-0000-0000-000000000000")
protocol IInterface7: IUnknown { }
// expected-error@-2 {{'@com' on a protocol requires 'interface:' argument}}

@com(interface: "00000000-0000-0000-0000-000000000008",
     implementation: "00000000-0000-0000-0000-000000000009")
protocol IInterface8: IUnknown { }
// expected-error@-3 {{'@com' on a protocol must only have 'interface:'; 'CLSID' is not allowed}}

@com(interface: "00000000-0000-0000-0000-000000000010",
     implementation: "")
protocol IInterface9: IUnknown { }
// expected-error@-3 {{'@com' on a protocol must only have 'interface:'; 'CLSID' is not allowed}}

@com(interface: "00000000-0000-0000-0000-000000000000")
class CClass10 { }
// expected-error@-2 {{'@com' on a class must not have 'interface:'; use 'implementation:' instead}}

@com(implementation: "guid")
class CClass11 { }
// expected-error@-2 {{'guid' is not a valid GUID}}

@com(implementation: "")
class CClass12 { }
// expected-error@-2 {{'' is not a valid GUID}}

// Declaration classification admits only a valid @com(interface:) protocol.
// In particular, the presence of a malformed attribute must not unlock the
// otherwise-reserved protocol metatype extension feature.
extension IInterface1.Protocol {}
extension IInterface4.Protocol {}
// expected-error@-1 {{protocol metatype extensions are reserved for COM interop}}
extension IInterface8.Protocol {}
// expected-error@-1 {{protocol metatype extensions are reserved for COM interop}}
