// RUN: %target-repl-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_repl

import Cocoa

// CHECK: 0{{$}}
print(NSNumber(value: 0).description)

protocol Q { func foo() }

extension CGRect: Q {
  func foo() {
    print(self)
  }
}

(CGRect() as Any as! Q).foo()
// CHECK: (0.0, 0.0, 0.0, 0.0)
