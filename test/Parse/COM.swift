// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -parse -verify %s -I %t

@com(interface: "00000000-0000-0000-C000-000000000046")
protocol IUnknown: AnyObject { }

@com
class C0 { }

@com(implementation: "00000000-0000-0000-0000-000000000001")
class C1 { }

@com(implementation: "00000000-0000-0000-0000-000000000002", threading: .free)
class C2 { }

@com(implementation: "00000000-0000-0000-0000-000000000003",
     threading: COMThreadingModel.apartment)
class C3 { }

@com(implementation: "00000000-0000-0000-0000-000000000004",
     threading: COM::COMThreadingModel.neutral)
class C4 { }

@com(implementation: "00000000-0000-0000-0000-000000000005", threading: .single)
class C5 { }

@com(implementation: "00000000-0000-0000-0000-000000000006", threading: .both)
class C6 { }

@com(implementation: "00000000-0000-0000-0000-000000000007", threading: .sta)
class C7 { }

@com(implementation: "00000000-0000-0000-0000-000000000008", threading: .mta)
class C8 { }


@com()
protocol P1 { }
// expected-error@-2 {{expected 'interface:', 'implementation:', or 'threading:' in '@com' attribute}}

@com(interface "")
protocol P2 { }
// expected-error@-2 {{expected ':' after label 'interface'}}

@com(interface: 42)
protocol P3 { }
// expected-error@-2 {{expected string literal in 'com' attribute}}

@com(label: "label")
protocol P4 { }
// expected-error@-2 {{unknown label 'label' in '@com' attribute; expected 'interface:', 'implementation:', or 'threading:'}}

@com(implementation: 42)
protocol P5 { }
// expected-error@-2 {{expected string literal in 'com' attribute}}

@com(implementation: "00000000-0000-0000-0000-000000000009", threading: apartment)
protocol P6 { }
// expected-error@-2 {{expected threading model (.single, .apartment, .free, .both, or .neutral}}

@com(implementation: "00000000-0000-0000-0000-000000000010", threading: .unknown)
protocol P7 { }
// expected-error@-2 {{unknown threading model 'unknown'; expected 'single', 'apartment', 'free', 'both', or 'neutral'}}
