// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

let opts: BinarySearchingOptions = [.firstEqual, .insertionIndex]

// CHECK: true
print(opts.intersection([.lastEqual, .insertionIndex]) == .insertionIndex)
// CHECK: false
print(!opts.intersection(.lastEqual).isEmpty)

// CHECK: {{^}}0 0{{$}}
print("\(([] as BinarySearchingOptions).rawValue) \(BinarySearchingOptions(rawValue: 0).rawValue)")
