// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface

public enum MyError: Error {
  case fail
}

// CHECK: #if compiler(>=5.3) && $TypedThrows
// CHECK-NEXT: public func throwsMyError() throws(Test.MyError)
// CHECK-NEXT: #endif
public func throwsMyError() throws(MyError) { }
