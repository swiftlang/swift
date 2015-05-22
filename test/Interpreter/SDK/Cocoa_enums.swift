// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

let opts: NSBinarySearchingOptions = [.FirstEqual, .InsertionIndex]

// CHECK: true
print(opts.intersect([.LastEqual, .InsertionIndex]) == .InsertionIndex)
// CHECK: false
print(!opts.intersect(.LastEqual).isEmpty)

// CHECK: {{^}}0 0{{$}}
print("\(([] as NSBinarySearchingOptions).rawValue) \(NSBinarySearchingOptions(rawValue: 0).rawValue)")
