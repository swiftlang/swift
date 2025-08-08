// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// REQUIRES: objc_interop

import Foundation

autoreleasepool {
  let f: @convention(block) (Int) -> Int = { $0 }
  // In an -Onone build this instantiates the generic metadata for
  // @convention(block) Int -> Int
  let ff: (@convention(block) (Int) -> Int)? = f
  let gg = ff

  // CHECK: 219
  print(gg!(219))
}
