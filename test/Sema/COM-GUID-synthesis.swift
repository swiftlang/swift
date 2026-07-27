// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM

// --- Form 1: @com(interface: "...") on a protocol synthesizes IID in extension P.Protocol

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget: IUnknown { }

let _: GUID = IWidget.IID

// --- Form 2: @com(implementation: "...") on a class synthesizes static var CLSID

@com(implementation: "20000000-0000-0000-0000-000000000002")
class Widget: IWidget { }

let _: GUID = Widget.CLSID

// --- IID is not synthesized for classes; CLSID is not on protocols

let _ = Widget.IID // expected-error {{type 'Widget' has no member 'IID'}}
let _ = IWidget.CLSID // expected-error {{type 'any IWidget' has no member 'CLSID'}}

// --- Form 3: bare @com on a class does not synthesize CLSID

@com
class BareWidget { }

let _ = BareWidget.CLSID // expected-error {{type 'BareWidget' has no member 'CLSID'}}

// --- IID is not inherited by conforming types

class ConcreteWidget: IWidget { }
let _ = ConcreteWidget.IID // expected-error {{type 'ConcreteWidget' has no member 'IID'}}

// --- Well-known protocols from the COM module

let _: GUID = IUnknown.IID
let _: GUID = ISwiftObject.IID
