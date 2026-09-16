// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -emit-module-path %t/ForeignClass.swiftmodule -module-name ForeignClass %S/Inputs/com_foreign_class.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM
import ForeignClass

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol ILocal {}

extension Foreign: ILocal {}
// expected-error@-1 {{conformance of 'Foreign' to COM interface 'ILocal' must be declared in the same module as 'Foreign'}}
