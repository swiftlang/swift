// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule
// RUN: mkdir -p %t/ReexporterModule

// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -emit-object -o %t/InternalModule/Internal.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ReexporterModule/Reexporter.swiftmodule %t/Reexporter.swift -I %t/IntermediateModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/ReexporterModule/Reexporter.o

// Test that when a hidden type is transitively re-exported (A -> B -> C) and
// the client imports the original internal module, the hidden type XREF
// resolves successfully and the IR uses the real mangled type name.
//
// Note: IntermediateType itself remains hidden (the client doesn't import
// IntermediateModule), but InternalType should be fully recovered through
// the transitive XREF chain.

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/ReexporterModule -I %t/InternalModule -o %t/Client.ll
// RUN: %FileCheck %s < %t/Client.ll

// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/ReexporterModule -I %t/InternalModule %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o %t/ReexporterModule/Reexporter.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// REQUIRES: executable_test
// REQUIRES: swift_feature_SafeImplementationOnly

//--- Internal.swift

public struct InternalType {
  public var x: Int64 = 1
  public var y: Int64 = 2

  public init() {}
}

//--- Intermediate.swift

@_implementationOnly import Internal

public struct IntermediateType {
  private var hidden: InternalType = InternalType()
  public var visible: Int64 = 3

  public init() {}

  public func getHiddenX() -> Int64 {
    return hidden.x
  }

  public func getHiddenY() -> Int64 {
    return hidden.y
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

  public func getHiddenX() -> Int64 {
    return intermediate.getHiddenX()
  }

  public func getHiddenY() -> Int64 {
    return intermediate.getHiddenY()
  }
}

//--- Client.swift

import Reexporter
import Internal

var instance = ReexporterType()

assert(instance.myVisible == 4)
assert(instance.getIntermediateVisible() == 3)
assert(instance.getHiddenX() == 1)
assert(instance.getHiddenY() == 2)

// When the client imports InternalModule, the XREF for InternalType resolves
// successfully even through the transitive re-export chain (A -> B -> C).
// InternalType appears as its real mangled name.
// CHECK-DAG: %T8Internal0A4TypeV = type <{ %Ts5Int64V, %Ts5Int64V }>

// IntermediateType is still hidden (client doesn't import Intermediate),
// but its layout should reference the recovered InternalType.
// CHECK-DAG: %T23s12Intermediate0A4TypeVXHn = type <{ %T8Internal0A4TypeV, %Ts5Int64V }>
