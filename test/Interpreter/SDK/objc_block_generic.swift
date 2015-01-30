// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

autoreleasepool {
  let f: @objc_block Int -> Int = { $0 }
  // In an -Onone build this instantiates the generic metadata for
  // @objc_block Int -> Int
  let ff: (@objc_block Int -> Int)? = f
  let gg = ff

  // CHECK: 219
  println(gg!(219))
}
