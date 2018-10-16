// RUN: %target-repl-run-simple-swift | %FileCheck %s

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

// Test the "mayLieAboutNonOptionalReturn" hack for both imported and
// non-imported types.
struct Empty {}
let _: Optional = Empty()
// CHECK: Optional(REPL_{{.+}}.Empty())
let _: Optional = CGPoint.zero
// CHECK: Optional((0.0, 0.0))
let _: Optional = NSString.availableStringEncodings
// CHECK: Optional(0x{{[0-9a-fA-F]+}})

