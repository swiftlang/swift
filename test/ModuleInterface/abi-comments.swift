// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/ABIComments.swiftinterface -module-name ABIComments -abi-comments-in-module-interface %s

// RUN: %FileCheck %s < %t/ABIComments.swiftinterface

// CHECK: // MANGLED NAME: $s11ABIComments11intToStringySSSiF
// CHECK-NEXT: public func intToString(_ value: Swift.Int) -> Swift.String
public func intToString(_ value: Int) -> String { "\(value)" }

// CHECK: // MANGLED NAME: $s11ABIComments8MyStructVMa
// CHECK-NEXT: public struct MyStruct {
public struct MyStruct {
  // CHECK: // MANGLED NAME: $s11ABIComments8MyStructV6methodSiyF
  // CHECK-NEXT: public func method() -> Swift.Int
  public func method() -> Int { 5 }
}
