// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

let opts: NSBinarySearchingOptions = .FirstEqual | .InsertionIndex

// CHECK: true
println((opts & (.LastEqual | .InsertionIndex)).getLogicValue())
// CHECK: false
println((opts & .LastEqual).getLogicValue())
