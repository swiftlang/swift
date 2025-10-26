// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-module %s -enable-experimental-feature Lifetimes -enable-experimental-feature LifetimeDependence -o %t/%target-library-name(Test) -module-name Test -emit-library

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s -enable-experimental-feature Lifetimes -enable-experimental-feature LifetimeDependence
// RUN: %swift-function-caller-generator Test %t/Test.swiftinterface > %t/out.swift

// RUN: %target-swift-frontend-verify -typecheck -strict-memory-safety %t/out.swift -I %t

func foo(x: Int) -> Int {
  return x
}

func bar(_ y: UnsafePointer<CInt>) {}

@_lifetime(borrow z)
func baz(_ z: Span<CInt>) -> Span<CInt> {
  return z
}

@_lifetime(`func`: copy `func`)
func qux(_ func: inout MutableSpan<CInt>) {}

