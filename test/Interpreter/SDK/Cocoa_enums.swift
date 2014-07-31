// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

let opts: NSBinarySearchingOptions = .FirstEqual | .InsertionIndex

// CHECK: true
println(opts & (.LastEqual | .InsertionIndex) == .InsertionIndex)
// CHECK: false
println((opts & .LastEqual) != nil)

// CHECK: {{^}}0 0 0{{$}}
println("\((nil as NSBinarySearchingOptions).toRaw()) \(NSBinarySearchingOptions.allZeros.toRaw()) \(NSBinarySearchingOptions.fromMask(0).toRaw())")
