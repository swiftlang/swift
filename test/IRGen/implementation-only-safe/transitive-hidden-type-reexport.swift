// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule
// RUN: mkdir -p %t/ReexporterModule

// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -emit-object -o %t/InternalModule/Internal.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ReexporterModule/Reexporter.swiftmodule %t/Reexporter.swift -I %t/IntermediateModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/ReexporterModule/Reexporter.o
// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/ReexporterModule %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o %t/ReexporterModule/Reexporter.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// Test transitive re-serialization of hidden type layout info.
// Module B @_implementationOnly imports A, and Module C @_implementationOnly
// imports B. The hidden layout info from A must survive through B into C's
// swiftmodule so that the client can correctly manipulate instances.

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

var instance = ReexporterType()

assert(instance.myVisible == 4)
assert(instance.getIntermediateVisible() == 3)
assert(instance.getHiddenX() == 1)
assert(instance.getHiddenY() == 2)

instance.myVisible = 40

assert(instance.myVisible == 40)
assert(instance.getIntermediateVisible() == 3)
assert(instance.getHiddenX() == 1)
assert(instance.getHiddenY() == 2)
