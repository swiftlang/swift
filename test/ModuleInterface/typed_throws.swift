// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface

public enum MyError: Error {
  case fail
}

// CHECK: public func throwsMyError() throws(Test.MyError)
public func throwsMyError() throws(MyError) { }

// CHECK: public func takesClosureThrowingMyError(_ closure: () throws(Test.MyError) -> Swift.Void)
public func takesClosureThrowingMyError(_ closure: () throws(MyError) -> Void) {}

public struct HasThrowingInit {
  // CHECK: public init() throws(Test.MyError)
  public init() throws(MyError) { }
}
