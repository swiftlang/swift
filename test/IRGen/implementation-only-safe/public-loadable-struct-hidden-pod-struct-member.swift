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

// A small integer-only POD struct.
public struct SmallIntPOD {
  public var x: Int64 = 1

  public init() {}
}

// A small floating-point POD struct.
public struct SmallFloatPOD {
  public var x: Double = 3.14

  public init() {}
}

// A small SIMD POD struct.
public struct SmallSIMDPOD {
  public var x: SIMD4<Float> = SIMD4<Float>(1.0, 2.0, 3.0, 4.0)

  public init() {}
}

//--- Intermediate.swift

@_implementationOnly import Internal

// Public struct wrapping a hidden integer POD field.
public struct PublicIntWrapper {
  private var hidden: SmallIntPOD = SmallIntPOD()
  public var visible: Int64 = 2

  public func getHiddenValue() -> Int64 {
    return hidden.x
  }

  public init() {}
}

public func makePublicIntWrapper() -> PublicIntWrapper {
  return PublicIntWrapper()
}

// Public struct wrapping a hidden floating-point POD field.
public struct PublicFloatWrapper {
  private var hidden: SmallFloatPOD = SmallFloatPOD()
  public var visible: Int64 = 2

  public func getHiddenValue() -> Double {
    return hidden.x
  }

  public init() {}
}

public func makePublicFloatWrapper() -> PublicFloatWrapper {
  return PublicFloatWrapper()
}

// Public struct wrapping a hidden SIMD POD field.
public struct PublicSIMDWrapper {
  private var hidden: SmallSIMDPOD = SmallSIMDPOD()
  public var visible: Int64 = 2

  public func getHiddenValue() -> SIMD4<Float> {
    return hidden.x
  }

  public init() {}
}

public func makePublicSIMDWrapper() -> PublicSIMDWrapper {
  return PublicSIMDWrapper()
}

//--- Client.swift

import Intermediate

var intS = makePublicIntWrapper()
assert(intS.visible == 2)
assert(intS.getHiddenValue() == 1)

var floatS = makePublicFloatWrapper()
assert(floatS.visible == 2)
assert(floatS.getHiddenValue() == 3.14)

var simdS = makePublicSIMDWrapper()
assert(simdS.visible == 2)
assert(simdS.getHiddenValue() == SIMD4<Float>(1.0, 2.0, 3.0, 4.0))

// Verify that the client's LLVM struct types for the hidden fields accurately
// reflect the layout of the original internal types. The client cannot see
// the internal type definitions, but the hidden layout names identify the
// original type and the field layout must match.

// SmallIntPOD has one Int64 field:
// CLIENT-DAG: [[INT:%T24s8Internal11SmallIntPODVXHn]] = type <{ %Ts5Int64V }>
// SmallFloatPOD has one Double field:
// CLIENT-DAG: [[FLOAT:%T26s8Internal13SmallFloatPODVXHn]] = type <{ %TSd }>
// SmallSIMDPOD has one SIMD4<Float> field:
// CLIENT-DAG: [[SIMD:%T25s8Internal12SmallSIMDPODVXHn]] = type <{ %Ts5SIMD4VySfG }>

// The public wrapper structs should contain the hidden struct followed by the
// visible Int64 member:
// CLIENT-DAG: %T12Intermediate16PublicIntWrapperV = type <{ [[INT]], %Ts5Int64V }>
// CLIENT-DAG: %T12Intermediate18PublicFloatWrapperV = type <{ [[FLOAT]], %Ts5Int64V }>
// CLIENT-DAG: %T12Intermediate17PublicSIMDWrapperV = type <{ [[SIMD]], %Ts5Int64V }>

// Verify that the calling conventions for makePublic* functions match between
// the intermediate module and the client module.

// INTERMEDIATE: define {{.*}}swiftcc { i64, i64 } @"$s12Intermediate20makePublicIntWrapperAA0cdE0VyF"()
// CLIENT: declare {{.*}}swiftcc { i64, i64 } @"$s12Intermediate20makePublicIntWrapperAA0cdE0VyF"()

// INTERMEDIATE: define {{.*}}swiftcc { double, i64 } @"$s12Intermediate22makePublicFloatWrapperAA0cdE0VyF"()
// CLIENT: declare {{.*}}swiftcc { double, i64 } @"$s12Intermediate22makePublicFloatWrapperAA0cdE0VyF"()

// INTERMEDIATE: define {{.*}}swiftcc { <4 x float>, i64 } @"$s12Intermediate21makePublicSIMDWrapperAA0cD0VyF"()
// CLIENT: declare {{.*}}swiftcc { <4 x float>, i64 } @"$s12Intermediate21makePublicSIMDWrapperAA0cD0VyF"()
