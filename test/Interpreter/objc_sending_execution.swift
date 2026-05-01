// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/objc_sending_execution.m -c -o %t/objc_sending_execution.o
// RUN: %target-build-swift -import-objc-header %S/Inputs/objc_sending_execution.h -Xlinker %t/objc_sending_execution.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out


// ---------------------------------------------------
// NOTE: Same as above, but with optimizations enabled

// RUN: %empty-directory(%t)
//
// RUN: %target-clang -O3 -fobjc-arc %S/Inputs/objc_sending_execution.m -c -o %t/objc_sending_execution.o
// RUN: %target-build-swift -O -import-objc-header %S/Inputs/objc_sending_execution.h -Xlinker %t/objc_sending_execution.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var ObjcSendingTestSuite = TestSuite("Sending")

class MyDelegate: NSObject, FooDelegate {
  func taking(_ object: sending NSObject) -> String {
    print("in MyDelegate.taking")
    return "hello! " + object.description
  }

  func identityFn(_ object: sending NSObject) -> sending NSObject {
    print("in MyDelegate.identityFn")
    return object
  }
}

ObjcSendingTestSuite.test("ensure no double free") {
  let foo = Foo()
  let delegate = MyDelegate()
  foo.delegate = delegate
  let ans = foo.doSomething()
  print(ans)
  expectEqual(ans.contains("hello!"), true)
  expectEqual(ans.contains("NSObject"), true)
}


// ----------------------------------------------------
// From https://github.com/swiftlang/swift/issues/87659

public final class SwiftHandler: NSObject, @unchecked Sendable, Handler {
  public func handle(_ value: sending Payload) {
    print("SwiftHandler received: \(value.name ?? "nil")")
  }
}
func getHandler(useSwift: Bool) -> any Handler {
  if useSwift {
    return SwiftHandler()
  } else {
    return LegacyObjCHandler()
  }
}

@inline(never)
func entryPoint(useSwift: Bool) {
  let handler: any Handler = getHandler(useSwift: useSwift)
  let obj = Payload()
  obj.name = "test-intent"
  handler.handle(obj)
}

ObjcSendingTestSuite.test("Crasher from https://github.com/swiftlang/swift/issues/87659") {
  MainActor.assumeIsolated {
    entryPoint(useSwift: false)
    entryPoint(useSwift: true)
  }
}


runAllTests()
