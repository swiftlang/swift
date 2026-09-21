// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -emit-module-path %t/BinaryBase.swiftmodule -module-name BinaryBase -enable-experimental-com-interop -I %t %t/BinaryBase.swift
// RUN: %target-swift-frontend -typecheck -module-name BinaryClient -enable-experimental-com-interop -I %t %t/BinaryClient.swift
// RUN: %target-swift-frontend -compile-module-from-interface %t/Base.swiftinterface -o %t/Base.swiftmodule -enable-experimental-com-interop -I %t
// RUN: %target-swift-frontend -compile-module-from-interface %t/Derived.swiftinterface -o %t/Derived.swiftmodule -enable-experimental-com-interop -I %t
// RUN: %target-swift-frontend -typecheck -enable-experimental-com-interop -I %t %t/Client.swift

//--- BinaryBase.swift
import COM

@com(interface: "31000000-0000-0000-0000-000000000001")
public protocol IBinaryBase {}

@com(interface: "31000000-0000-0000-0000-000000000002")
public protocol IBinaryMiddle: IBinaryBase {}

//--- BinaryClient.swift
import COM
import BinaryBase

@com(interface: "31000000-0000-0000-0000-000000000003")
protocol IBinaryLeaf: IBinaryMiddle, IBinaryBase {}

//--- Base.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Base -language-mode 5 -enable-library-evolution -enable-experimental-com-interop
import COM

@com(interface: "30000000-0000-0000-0000-000000000001")
public protocol IBase {}

@com(interface: "30000000-0000-0000-0000-000000000002")
public protocol IMiddle: IBase {}

//--- Derived.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Derived -language-mode 5 -enable-library-evolution -enable-experimental-com-interop
import COM
import Base

@com(interface: "30000000-0000-0000-0000-000000000003")
public protocol ILeaf: IMiddle, IBase, Sendable {}

//--- Client.swift
import COM
import Derived

func use(_ value: any ILeaf) {}
