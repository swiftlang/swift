// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=microsoft %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -I %t

// expected-note@+1 2{{required by global function}}
func interface<Identity: COMInterface>(_ type: Identity) -> IID { type.IID }
// expected-note@+1 3{{required by global function}}
func activatable<Identity: COMActivatable>(_ type: Identity) {}

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
protocol IWidget: IUnknown {}

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f002")
protocol IRefinedWidget: IWidget {}

@com(implementation: "01020304-0506-0708-090a-0b0c0d0e0f10")
class Widget: IWidget {}

@com
class UnidentifiedWidget: IWidget {}

protocol OrdinaryProtocol {}
class OrdinaryClass {}

let _: IID = interface(IWidget.self)
let _: IID = interface(IRefinedWidget.self)
let _: IID = interface((any IWidget & Sendable).self)
activatable(Widget.self)

func reject(_ widget: any IWidget) {
  _ = interface(widget)
// expected-error@-1 {{requires that 'Identity' conform to 'COMInterface'}}
}
_ = interface(Widget.self)
// expected-error@-1 {{type 'Widget.Type' cannot conform to 'COMInterface'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
_ = interface(OrdinaryProtocol.self)
// expected-error@-1 {{type '(any OrdinaryProtocol).Type' cannot conform to 'COMInterface'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
activatable(IWidget.self)
// expected-error@-1 {{type '(any IWidget).Type' cannot conform to 'COMActivatable'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
activatable(UnidentifiedWidget.self)
// expected-error@-1 {{type 'UnidentifiedWidget.Type' cannot conform to 'COMActivatable'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
activatable(OrdinaryClass.self)
// expected-error@-1 {{type 'OrdinaryClass.Type' cannot conform to 'COMActivatable'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
