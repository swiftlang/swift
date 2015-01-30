// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

let opts: NSBinarySearchingOptions = .FirstEqual | .InsertionIndex

// CHECK: true
println(opts & (.LastEqual | .InsertionIndex) == .InsertionIndex)
// CHECK: false
println((opts & .LastEqual) != nil)

// CHECK: {{^}}0 0 0{{$}}
println("\((nil as NSBinarySearchingOptions).rawValue) \(NSBinarySearchingOptions.allZeros.rawValue) \(NSBinarySearchingOptions(0).rawValue)")
