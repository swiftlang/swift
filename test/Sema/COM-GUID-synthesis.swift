// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=microsoft %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -I %t

import COM

// --- An interface ID is available through its metatype conformance

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget: IUnknown { }

let _: GUID = IWidget.IID

// --- A class implementation ID is available through its metatype conformance

@com(implementation: "20000000-0000-0000-0000-000000000002")
class Widget: IWidget { }

let _: GUID = Widget.CLSID

// --- Classes have no interface identity; interfaces have no activation identity

let _ = Widget.IID // expected-error {{type 'Widget' has no member 'IID'}}
let _ = IWidget.CLSID // expected-error {{type 'any IWidget' has no member 'CLSID'}}

// --- Bare @com does not provide an activation identity

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

// Activation identity belongs to the declaring class, not its subclasses.
class UnidentifiedSubclass: Widget {}
let _ = UnidentifiedSubclass.CLSID
// expected-error@-1 {{type 'UnidentifiedSubclass' has no member 'CLSID'}}

@com(implementation: "30000000-0000-0000-0000-000000000003")
class IdentifiedSubclass: Widget {}
let _: CLSID = IdentifiedSubclass.CLSID

// An interface conformance does not imply a metatype identity conformance.
func generic<Interface: IUnknown>(_: Interface.Type) {
  _ = Interface.IID
  // expected-error@-1 {{type 'Interface' has no member 'IID'}}
}
