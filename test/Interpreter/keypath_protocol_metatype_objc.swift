// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// Key paths rooted at @objc protocol existentials and existential metatypes.

@objc protocol OP {
  var v: Int { get }
  static var sv: Int { get }
}

class C1: NSObject, OP {
  var v: Int { 1 }
  static var sv: Int { 10 }
}
class C2: NSObject, OP {
  var v: Int { 2 }
  static var sv: Int { 20 }
}

let iv: KeyPath<any OP, Int> = \.v
let sv: KeyPath<any OP.Type, Int> = \.sv

let pairs: [(any OP, any OP.Type)] = [(C1(), C1.self), (C2(), C2.self)]
for (e, m) in pairs {
  print(e[keyPath: iv], m[keyPath: sv])
}
// CHECK: 1 10
// CHECK: 2 20
