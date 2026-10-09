// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -emit-module-path %t/COM.swiftmodule -module-name COM -Xfrontend -enable-experimental-com-interop -Xfrontend -com-interop-model=microsoft %S/../Inputs/COM.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(Identities)) -emit-module-path %t/Identities.swiftmodule -module-name Identities -Xfrontend -enable-experimental-com-interop -Xfrontend -com-interop-model=microsoft -I %t -L %t -lCOM %target-rpath(%t) %t/Identities.swift
// RUN: %target-build-swift %t/main.swift -o %t/test -Xfrontend -enable-experimental-com-interop -Xfrontend -com-interop-model=microsoft -Xfrontend -sil-verify-all -I %t -L %t -lCOM -lIdentities %target-rpath(%t)
// RUN: %target-codesign %t/test %t/%target-library-name(COM) %t/%target-library-name(Identities)
// RUN: %target-run %t/test %t/%target-library-name(COM) %t/%target-library-name(Identities) | %FileCheck %s
// RUN: %target-build-swift -O %t/main.swift -o %t/test-opt -Xfrontend -enable-experimental-com-interop -Xfrontend -com-interop-model=microsoft -Xfrontend -sil-verify-all -I %t -L %t -lCOM -lIdentities %target-rpath(%t)
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) %t/%target-library-name(Identities) | %FileCheck %s
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

// CHECK: identities match

//--- Identities.swift
@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
public protocol IWidget: IUnknown {}

@com(interface: "11223344-5566-7788-99aa-bbccddeeff00")
public protocol IRefinedWidget: IWidget {}

@com(implementation: "01020304-0506-0708-090a-0b0c0d0e0f10")
public class Widget: IWidget {}

@inline(never)
public func interfaceID<Identity: COMInterface>(_ type: Identity) -> IID {
  type.IID
}

@inline(never)
public func activationID<Identity: COMActivatable>(_ type: Identity) -> CLSID {
  type.CLSID
}

@inline(never)
public func interfaceIDClosure<Identity: COMInterface>(_ type: Identity)
    -> () -> IID {
  { type.IID }
}

@inline(never)
public func activationIDKeyPath<Identity: COMActivatable>(_ type: Identity)
    -> CLSID {
  type[keyPath: \Identity.CLSID]
}

@_alwaysEmitIntoClient
public func inlinedInterfaceID() -> IID { interfaceID(IWidget.self) }

@_alwaysEmitIntoClient
public func inlinedActivationID() -> CLSID { activationID(Widget.self) }

//--- main.swift
import Identities

func localInterfaceID<Identity: COMInterface>(_ type: Identity) -> IID {
  type.IID
}

func check(_ id: GUID, _ data1: UInt32, _ data2: UInt16, _ data3: UInt16,
           _ data4: [UInt8]) {
  precondition(id.data1 == data1 && id.data2 == data2 && id.data3 == data3)
  withUnsafeBytes(of: id.data4) { precondition(Array($0) == data4) }
}

check(interfaceID(IWidget.self), 0x10203040, 0x5060, 0x7080,
      [0x90, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0, 0x01])
check(interfaceID(IRefinedWidget.self), 0x11223344, 0x5566, 0x7788,
      [0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00])
check(interfaceID((any IWidget & Sendable).self), 0x10203040, 0x5060, 0x7080,
      [0x90, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0, 0x01])
check(activationID(Widget.self), 0x01020304, 0x0506, 0x0708,
      [0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10])
check(inlinedInterfaceID(), 0x10203040, 0x5060, 0x7080,
      [0x90, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0, 0x01])
check(inlinedActivationID(), 0x01020304, 0x0506, 0x0708,
      [0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10])
check(interfaceIDClosure(IWidget.self)(), 0x10203040, 0x5060, 0x7080,
      [0x90, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0, 0x01])
check(activationIDKeyPath(Widget.self), 0x01020304, 0x0506, 0x0708,
      [0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10])
check(localInterfaceID(IWidget.self), 0x10203040, 0x5060, 0x7080,
      [0x90, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0, 0x01])
print("identities match")
