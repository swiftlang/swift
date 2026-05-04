// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule

// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -emit-object -o %t/InternalModule/Internal.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o

// Test that when the client imports the internal module, hidden type XREFs
// for sibling hidden types resolve successfully. OuterHidden contains a field
// of type InnerHidden — both from the same @_implementationOnly module. When
// the client can see Internal, both should recover to their real types.

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/IntermediateModule -I %t/InternalModule -o %t/Client.ll
// RUN: %FileCheck %s < %t/Client.ll

// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/IntermediateModule -I %t/InternalModule %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// REQUIRES: executable_test
// REQUIRES: swift_feature_SafeImplementationOnly

//--- Internal.swift

public struct InnerHidden {
  public var value: Int64 = 7

  public init() {}
}

public struct OuterHidden {
  public var inner: InnerHidden = InnerHidden()
  public var extra: Int64 = 13

  public init() {}
}

//--- Intermediate.swift

@_implementationOnly import Internal

public struct PublicWrapper {
  private var hidden: OuterHidden = OuterHidden()
  public var visible: Int64 = 2

  public func getInnerValue() -> Int64 {
    return hidden.inner.value
  }

  public func getExtraValue() -> Int64 {
    return hidden.extra
  }

  public init() {}
}

public func makePublicWrapper() -> PublicWrapper {
  return PublicWrapper()
}

//--- Client.swift

import Intermediate
import Internal

var s = makePublicWrapper()
assert(s.visible == 2)
assert(s.getInnerValue() == 7)
assert(s.getExtraValue() == 13)

// When the client imports InternalModule, both hidden types recover to their
// real types. InnerHidden and OuterHidden use their real mangled names.
// CHECK-DAG: %T8Internal11InnerHiddenV = type <{ %Ts5Int64V }>
// CHECK-DAG: %T8Internal11OuterHiddenV = type <{ %T8Internal11InnerHiddenV, %Ts5Int64V }>
// CHECK-DAG: %T12Intermediate13PublicWrapperV = type <{ %T8Internal11OuterHiddenV, %Ts5Int64V }>
