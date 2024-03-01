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

// CHECK:      #if compiler(>=5.3) && $TypedThrows
// CHECK-NEXT: public func takesClosureThrowingMyError(_ closure: () throws(Test.MyError) -> Swift.Void)
// CHECK-NEXT: #endif
public func takesClosureThrowingMyError(_ closure: () throws(MyError) -> Void) {}

public struct HasThrowingInit {
  // CHECK:      #if compiler(>=5.3) && $TypedThrows
  // CHECK-NEXT: public init() throws(Test.MyError)
  // CHECK-NEXT: #endif
  public init() throws(MyError) { }
}
