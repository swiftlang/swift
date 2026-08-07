// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -emit-module-path %t/Interfaces.swiftmodule -module-name Interfaces -enable-experimental-com-interop -I %t %t/Interfaces.swift
// RUN: %target-swift-frontend -typecheck -enable-experimental-com-interop -I %t %t/client.swift

//--- Interfaces.swift
@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
public protocol IWidget: IUnknown { }

@com(interface: "a1a2a3a4-b1b2-c1c2-d1d2-e1e2e3e4e5e6")
public protocol IRefinedWidget: IWidget { }

//--- client.swift
import Interfaces

struct TearOff<Interface> where Interface.Type: COMInterface {
  static var IID: IID { Interface.IID }
}

let _: TearOff<IWidget>.Type = TearOff<IWidget>.self
let _: TearOff<IRefinedWidget>.Type = TearOff<IRefinedWidget>.self

func use() {
  _ = TearOff<IWidget>.IID
  _ = TearOff<IRefinedWidget>.IID
}
