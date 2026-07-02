// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -emit-object -o %t/InternalModule/Internal.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Intermediate.swift -I %t/InternalModule -o %t/Intermediate.ll
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/IntermediateModule -o %t/Client.ll
// RUN: %FileCheck %s --check-prefix=INTERMEDIATE < %t/Intermediate.ll
// RUN: %FileCheck %s --check-prefix=CLIENT < %t/Client.ll
// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/IntermediateModule  %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// UNSUPPORTED: CPU=wasm32
// REQUIRES: executable_test
// REQUIRES: swift_feature_SafeImplementationOnly

//--- Internal.swift

// A struct with 5 Int64 fields, too large to pass in registers (exceeds the
// 4-scalar explosion limit on x86_64), making it address-only.
public struct LargeInternalStruct {
  public var a: Int64 = 1
  public var b: Int64 = 2
  public var c: Int64 = 3
  public var d: Int64 = 4
  public var e: Int64 = 5

  public init() {}
}

//--- Intermediate.swift

@_implementationOnly import Internal

public struct PublicWrapper {
  private var hidden: LargeInternalStruct = LargeInternalStruct()
  public var visible: Int64 = 42

  public func getHiddenA() -> Int64 { return hidden.a }
  public func getHiddenE() -> Int64 { return hidden.e }

  public init() {}
}

public func makePublicWrapper() -> PublicWrapper {
  return PublicWrapper()
}

//--- Client.swift

import Intermediate

var s = makePublicWrapper()
assert(s.visible == 42)
assert(s.getHiddenA() == 1)
assert(s.getHiddenE() == 5)

// The struct is too large for registers, so makePublicWrapper returns via sret.
// INTERMEDIATE: define {{.*}}swiftcc void @"$s12Intermediate17makePublicWrapperAA0cD0VyF"(ptr noalias sret
// CLIENT: declare {{.*}}swiftcc void @"$s12Intermediate17makePublicWrapperAA0cD0VyF"(ptr noalias sret
