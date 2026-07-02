// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule

// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -emit-object -o %t/InternalModule/Internal.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o

// Test that when the client imports the internal module, the hidden type XREF
// resolves successfully and the IR uses the real mangled type name rather than
// the opaque "hidden_struct" placeholder.

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/IntermediateModule -I %t/InternalModule -o %t/Client.ll
// RUN: %FileCheck %s < %t/Client.ll

// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/IntermediateModule -I %t/InternalModule %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o -o %t/client-executable
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

//--- Client.swift

import Intermediate
import Internal

var instance = IntermediateType()

assert(instance.visible == 3)
assert(instance.getHiddenX() == 1)
assert(instance.getHiddenY() == 2)

// When the client imports InternalModule, the XREF resolves successfully
// and the real type is used instead of a hidden_struct placeholder.
// IntermediateType's layout references the recovered InternalType by its
// real mangled name.
// CHECK-DAG: %T8Internal0A4TypeV = type <{ %Ts5Int64V, %Ts5Int64V }>
// CHECK-DAG: %T12Intermediate0A4TypeV = type <{ %T8Internal0A4TypeV, %Ts5Int64V }>
