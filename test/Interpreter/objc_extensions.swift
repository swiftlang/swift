// RUN: %target-run-simple-swift %s | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

class OuterClass {
  class InnerClass: NSObject {}
}

extension OuterClass.InnerClass {
  @objc static let propertyInExtension = "foo"
}

let x = OuterClass.InnerClass()

// CHECK: foo
print(type(of: x).propertyInExtension)

