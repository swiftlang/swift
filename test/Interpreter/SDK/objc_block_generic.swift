// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

autoreleasepool {
  let f: @objc_block Int -> Int = { $0 }
  // In an -O0 build this instantiates the generic metadata for
  // @objc_block Int -> Int
  let ff: (@objc_block Int -> Int)? = f
  let gg = ff

  // CHECK: 219
  println(gg!(219))
}
