// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule

// Compile the internal module with library evolution to make its types resilient.
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -emit-object -o %t/InternalModule/Internal.o -enable-library-evolution

// Compile the intermediate module without library evolution.
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o

// Dump IR for inspection.
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Intermediate.swift -I %t/InternalModule -parse-as-library -o %t/Intermediate.ll
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/IntermediateModule -o %t/Client.ll
// RUN: %FileCheck %s < %t/Client.ll
// RUN: %FileCheck --check-prefix=CHECK-INTERMEDIATE %s < %t/Intermediate.ll

// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/IntermediateModule %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// UNSUPPORTED: CPU=wasm32
// REQUIRES: executable_test
// REQUIRES: swift_feature_SafeImplementationOnly

//--- Internal.swift

public struct ResilientPOD {
  public var x: Int64 = 42

  public init() {}
}

//--- Intermediate.swift

@_implementationOnly import Internal

public struct PublicWrapper {
  private var hidden: ResilientPOD = ResilientPOD()
  public var visible: Int64 = 2

  public func getHiddenValue() -> Int64 {
    return hidden.x
  }

  public init() {}
}

public func makePublicWrapper() -> PublicWrapper {
  return PublicWrapper()
}

//--- Client.swift

import Intermediate

func copyAndGetVisible(_ w: PublicWrapper) -> Int64 {
  var copy = w
  return copy.visible
}

var s = makePublicWrapper()
assert(s.visible == 2)
assert(s.getHiddenValue() == 42)
assert(copyAndGetVisible(s) == 2)

// makePublicWrapper returns indirectly via sret with swiftcc.
// CHECK-DAG: declare swiftcc void @"$s12Intermediate17makePublicWrapperAA0cD0VyF"(ptr noalias sret(%swift.opaque))
// CHECK-INTERMEDIATE: define {{.*}}swiftcc void @"$s12Intermediate17makePublicWrapperAA0cD0VyF"(ptr noalias sret(%swift.opaque) %0)

// The outlined copy helper calls the hidden type's metadata accessor,
// loads the VWT, and calls InitializeWithCopy through it.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$s12Intermediate13PublicWrapperVWOc"
// CHECK: call swiftcc %swift.metadata_response @"$s8Internal12ResilientPODVMa"(i64 0)
// CHECK: %InitializeWithCopy = load ptr
// CHECK: call ptr %InitializeWithCopy(

// The client calls the hidden type's metadata accessor to get VWT for
// copy/destroy of the resilient hidden field.
// CHECK-DAG: declare swiftcc %swift.metadata_response @"$s8Internal12ResilientPODVMa"(i64)

// The outlined destroy helper similarly uses the hidden type's VWT.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$s12Intermediate13PublicWrapperVWOh"
// CHECK: call swiftcc %swift.metadata_response @"$s8Internal12ResilientPODVMa"(i64 0)
// CHECK: %Destroy = load ptr
// CHECK: call void %Destroy(
