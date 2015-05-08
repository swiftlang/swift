// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

let opts: NSBinarySearchingOptions = .FirstEqual | .InsertionIndex

// CHECK: true
print(opts & (.LastEqual | .InsertionIndex) == .InsertionIndex)
// CHECK: false
print((opts & .LastEqual) != nil)

// CHECK: {{^}}0 0 0{{$}}
print("\((nil as NSBinarySearchingOptions).rawValue) \(NSBinarySearchingOptions.allZeros.rawValue) \(NSBinarySearchingOptions(0).rawValue)")
