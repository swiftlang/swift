// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface)
// RUN: %FileCheck %s < %t/Foo.swiftinterface

// CHECK: macCatalyst
@available(macOS 12.0, iOS 15.0, macCatalyst 15.0, *)
public struct Foo {}
