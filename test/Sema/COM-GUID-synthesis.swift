// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=microsoft %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -I %t

import COM

// --- Form 1: a `@com` interface metatype provides IID through COMInterface

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget: IUnknown { }

let _: GUID = IWidget.IID

// --- Form 2: an implementation metatype provides CLSID through COMActivatable

@com(implementation: "20000000-0000-0000-0000-000000000002")
class Widget: IWidget { }

let _: GUID = Widget.CLSID

func __uuidof<Implementation>(_ type: Implementation.Type) -> CLSID
    where Implementation.Type: COMActivatable {
  Implementation.CLSID
}

let _: GUID = __uuidof(Widget.self)

// --- IID is not available on implementation classes; CLSID is not on protocols

let _ = Widget.IID // expected-error {{type 'Widget' has no member 'IID'}}
let _ = IWidget.CLSID // expected-error {{type 'any IWidget' has no member 'CLSID'}}

// --- Form 3: bare @com on a class is not activatable

@com
class BareWidget { }

let _ = BareWidget.CLSID // expected-error {{type 'BareWidget' has no member 'CLSID'}}

// --- IID is not inherited by conforming types

class ConcreteWidget: IWidget { }
let _ = ConcreteWidget.IID // expected-error {{type 'ConcreteWidget' has no member 'IID'}}
let _ = ConcreteWidget.CLSID // expected-error {{type 'ConcreteWidget' has no member 'CLSID'}}

// --- Well-known protocols from the COM module

let _: GUID = IUnknown.IID
let _: GUID = ISwiftObject.IID
