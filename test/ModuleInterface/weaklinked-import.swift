// RUN: %empty-directory(%t)
//
// RUN: echo 'public struct S {}' > %t/Other.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Other.swiftmodule -parse-as-library %t/Other.swift -enable-library-evolution
//
// RUN: %target-swift-emit-module-interface(%t/WeakLinksOther.swiftinterface) %s -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/WeakLinksOther.swiftinterface) -I %t
// RUN: %FileCheck %s < %t/WeakLinksOther.swiftinterface

// UNSUPPORTED: OS=windows-msvc

// The @_weakLinked attribute should not be printed in swiftinterfaces.
// CHECK-NOT: @_weakLinked
@_weakLinked import Other
