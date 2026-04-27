// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/ResilientModule
// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule

// Compile a resilient module (library evolution) that provides a resilient type.
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/ResilientModule/Resilient.swiftmodule %t/Resilient.swift -parse-as-library -emit-object -o %t/ResilientModule/Resilient.o -enable-library-evolution -module-name Resilient

// Internal module imports the resilient module and wraps its type in a non-resilient struct.
// The wrapper struct has non-fixed layout because the resilient field's size is unknown at compile time.
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -I %t/ResilientModule -parse-as-library -emit-object -o %t/InternalModule/Internal.o -module-name Internal

// Intermediate module uses the internal module's non-fixed struct as a hidden field.
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -I %t/ResilientModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o -module-name Intermediate

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Intermediate.swift -I %t/InternalModule -I %t/ResilientModule -parse-as-library -o %t/Intermediate.ll
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/IntermediateModule -o %t/Client.ll
// RUN: %FileCheck %s < %t/Client.ll
// RUN: %FileCheck --check-prefix=CHECK-INTERMEDIATE %s < %t/Intermediate.ll

// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/IntermediateModule %t/ResilientModule/Resilient.o %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// UNSUPPORTED: CPU=wasm32
// REQUIRES: executable_test
// REQUIRES: swift_feature_SafeImplementationOnly

//--- Resilient.swift

public struct ResilientPOD {
  public var x: Int64 = 42
  public init() {}
}

//--- Internal.swift

import Resilient

// This struct is NOT resilient (no library evolution), but contains a
// resilient field, making its layout non-fixed at compile time.
public struct WrapperOfResilient {
  public var resilientField: ResilientPOD = ResilientPOD()
  public var extra: Int64 = 7

  public init() {}

  public func getResilientValue() -> Int64 { return resilientField.x }
  public func getExtra() -> Int64 { return extra }
}

//--- Intermediate.swift

@_implementationOnly import Internal

public struct PublicWrapper {
  private var hidden: WrapperOfResilient = WrapperOfResilient()
  public var visible: Int64 = 99

  public init() {}

  public func getHiddenResilientValue() -> Int64 {
    return hidden.getResilientValue()
  }

  public func getHiddenExtra() -> Int64 {
    return hidden.getExtra()
  }
}

public func makePublicWrapper() -> PublicWrapper {
  return PublicWrapper()
}

//--- Client.swift

import Intermediate

var w = makePublicWrapper()
assert(w.visible == 99)
assert(w.getHiddenResilientValue() == 42)
assert(w.getHiddenExtra() == 7)

// The hidden non-fixed struct makes PublicWrapper itself non-fixed.
// makePublicWrapper returns indirectly via sret since the layout is non-fixed.
// CHECK-DAG: declare swiftcc void @"$s12Intermediate17makePublicWrapperAA0cD0VyF"(ptr noalias sret(%swift.opaque))
// CHECK-INTERMEDIATE: define {{.*}}swiftcc void @"$s12Intermediate17makePublicWrapperAA0cD0VyF"(ptr noalias sret(%swift.opaque) %0)
