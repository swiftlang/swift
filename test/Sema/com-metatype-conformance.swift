// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

struct TearOff<Interface> where Interface.Type: COMInterface {
  static var IID: IID { Interface.IID }
}

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget: IUnknown { }

@com(interface: "20000000-0000-0000-0000-000000000002")
protocol IRefinedWidget: IWidget { }

@com(interface: "30000000-0000-0000-0000-000000000003")
protocol IInterface: IUnknown { }

protocol P: AnyObject { }

typealias WidgetTearOff = TearOff<IWidget>
typealias RefinedWidgetTearOff = TearOff<IRefinedWidget>
typealias SendableWidgetTearOff = TearOff<IWidget & Sendable>

func f<Interface>(_ value: borrowing Interface)
    where Interface.Type: COMInterface {
  _ = Interface.IID
}

typealias ErasedTearOff = TearOff<COMInterface>
// expected-error@-1{{'any COMInterface' is invalid because 'COMInterface' describes a COM metatype identity}}

typealias AmbiguousTearOff = TearOff<IWidget & IInterface>
// expected-error@-1{{COM existential cannot contain interfaces 'IInterface' and 'IWidget' because neither refines the other}}

typealias OrdinaryTearOff = TearOff<P>
// expected-error@-1{{type '(any P).Type' does not conform to protocol 'COMInterface'}}

final class Widget: IWidget { }

typealias ImplementationTearOff = TearOff<Widget>
// expected-error@-1{{type 'Widget.Type' does not conform to protocol 'COMInterface'}}

let _ = Widget.IID
// expected-error@-1{{type 'Widget' has no member 'IID'}}
