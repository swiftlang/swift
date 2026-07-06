// Tests that hidden representations are emitted for transitive dependencies,
// not just @_implementationOnly imported types. When a hidden type references
// a type from another module, that type also gets a hidden representation
// even if it was not @_implementationOnly imported.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/ModuleA
// RUN: mkdir -p %t/ModuleB
// RUN: mkdir -p %t/ModuleC

// UNSUPPORTED: CPU=wasm32

// Compile ModuleA — a regular public module.
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/ModuleA/A.swiftmodule %t/A.swift -parse-as-library -emit-object -o %t/ModuleA/A.o -module-name A

// Compile ModuleB — imports A normally, wraps A's type.
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/ModuleB/B.swiftmodule %t/B.swift -I %t/ModuleA -parse-as-library -emit-object -o %t/ModuleB/B.o -module-name B

// Compile ModuleC — imports B as @_implementationOnly.
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/ModuleC/C.swiftmodule %t/C.swift -I %t/ModuleB -I %t/ModuleA -parse-as-library -emit-object -o %t/ModuleC/C.o -module-name C

// Client imports only C — no -I for A or B.
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/ModuleC -o %t/Client.ll
// RUN: %FileCheck %s < %t/Client.ll

// Build and run to verify correctness.
// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/ModuleC %t/ModuleA/A.o %t/ModuleB/B.o %t/ModuleC/C.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// REQUIRES: executable_test
// REQUIRES: swift_feature_SafeImplementationOnly

//--- A.swift

public struct TypeFromA {
  public var value: Int64 = 42
  public init() {}
}

//--- B.swift

import A

public struct WrapperFromB {
  public var inner: TypeFromA = TypeFromA()
  public var extra: Int64 = 7
  public init() {}

  public func getInnerValue() -> Int64 { return inner.value }
  public func getExtra() -> Int64 { return extra }
}

//--- C.swift

@_implementationOnly import B

public struct PublicWrapper {
  private var hidden: WrapperFromB = WrapperFromB()
  public var visible: Int64 = 99

  public init() {}

  public func getHiddenInnerValue() -> Int64 {
    return hidden.getInnerValue()
  }

  public func getHiddenExtra() -> Int64 {
    return hidden.getExtra()
  }
}

public func makePublicWrapper() -> PublicWrapper {
  return PublicWrapper()
}

//--- Client.swift

import C

var w = makePublicWrapper()
assert(w.visible == 99)
assert(w.getHiddenInnerValue() == 42)
assert(w.getHiddenExtra() == 7)

// The hidden type makes PublicWrapper loadable with known layout.
// CHECK: declare swiftcc { i64, i64, i64 } @"$s1C17makePublicWrapperAA0bC0VyF"()
