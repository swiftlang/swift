// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule
// RUN: mkdir -p %t/ReexporterModule

// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -emit-object -o %t/InternalModule/Internal.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ReexporterModule/Reexporter.swiftmodule %t/Reexporter.swift -I %t/IntermediateModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/ReexporterModule/Reexporter.o

// Test that when a hidden NESTED type is transitively re-exported and the
// client imports the original internal module, the nested type XREF (which
// has multiple type path pieces) resolves successfully and the IR uses the
// real mangled type name.

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/ReexporterModule -I %t/IntermediateModule -I %t/InternalModule -o %t/Client.ll
// RUN: %FileCheck %s < %t/Client.ll

// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/ReexporterModule -I %t/IntermediateModule -I %t/InternalModule %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o %t/ReexporterModule/Reexporter.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// REQUIRES: executable_test
// REQUIRES: swift_feature_SafeImplementationOnly

//--- Internal.swift

public struct Outer {
  public struct Inner {
    public var a: Int64 = 10
    public var b: Int64 = 20

    public init() {}
  }
}

//--- Intermediate.swift

@_implementationOnly import Internal

public struct IntermediateType {
  private var hidden: Outer.Inner = Outer.Inner()
  public var visible: Int64 = 3

  public init() {}

  public func getHiddenA() -> Int64 {
    return hidden.a
  }

  public func getHiddenB() -> Int64 {
    return hidden.b
  }
}

//--- Reexporter.swift

@_implementationOnly import Intermediate

public struct ReexporterType {
  private var intermediate: IntermediateType = IntermediateType()
  public var myVisible: Int64 = 4

  public init() {}

  public func getIntermediateVisible() -> Int64 {
    return intermediate.visible
  }

  public func getHiddenA() -> Int64 {
    return intermediate.getHiddenA()
  }

  public func getHiddenB() -> Int64 {
    return intermediate.getHiddenB()
  }
}

//--- Client.swift

import Reexporter
import Intermediate
import Internal

var instance = ReexporterType()

assert(instance.myVisible == 4)
assert(instance.getIntermediateVisible() == 3)
assert(instance.getHiddenA() == 10)
assert(instance.getHiddenB() == 20)

// The nested type Outer.Inner is recovered via a multi-piece XREF path.
// CHECK-DAG: %T8Internal5OuterV5InnerV = type <{ %Ts5Int64V, %Ts5Int64V }>

// IntermediateType is still hidden but references the recovered nested type.
// CHECK-DAG: %T12Intermediate0A4TypeV = type <{ %T8Internal5OuterV5InnerV, %Ts5Int64V }>
