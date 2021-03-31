// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
  // This tests behavior that was fixed in
  // https://github.com/apple/swift/pull/36355

  let array = NSMutableArray()
  for _ in 0..<1000 {
    array.insert(NSObject(), at: 0)
  }

  // Check that this does not crash because of an over-release of the array content.
  _ = (array as! [NSObject]).prefix(3)
  _ = (array as! [NSObject]).prefix(3)
}

// CHECK: done
print("done")
