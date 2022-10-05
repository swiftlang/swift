// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/objc_implementation.h -D TOP_LEVEL_CODE -swift-version 5) %s | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

@_objcImplementation extension ImplClass {
  @objc class func runTests() {
    print(ImplClass().someMethod())
    print(SwiftSubclass().someMethod())
  }

  @objc func someMethod() -> String { "ImplClass.someMethod()" }
}

class SwiftSubclass: ImplClass {
  override func someMethod() -> String { "SwiftSubclass.someMethod()" }
}

// `#if swift` to ignore the inactive branch's contents
#if swift(>=5.0) && TOP_LEVEL_CODE
ImplClass.runTests()
// CHECK: ImplClass.someMethod()
// CHECK: SwiftSubclass.someMethod()
#endif
