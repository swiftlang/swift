// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Self -emit-module-path %t/Self.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name Self -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Self.symbols.json

public protocol P {}
extension P {
  static func foo(_ thing: Self) {}
}

public struct MyStruct: Equatable {
  public static func ==(lhs: MyStruct, rhs: MyStruct) -> Bool {
    return true
  }
}

// We don't want Self: Equatable to show up here for !=
// CHECK-NOT: swiftGenerics
