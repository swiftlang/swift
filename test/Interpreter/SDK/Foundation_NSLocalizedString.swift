// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

let key = "inquiens"
let value = "λαμπερός"

let s1 = NSLocalizedString(key)
// CHECK: key = inquiens s1 = inquiens
println("key = \(key) s1 = \(s1)")

let s2 = NSLocalizedString(key, tableName:nil, NSBundle.mainBundle(), value: value)
// CHECK: key = inquiens s2 = λαμπερός
println("key = \(key) s2 = \(s2)")

