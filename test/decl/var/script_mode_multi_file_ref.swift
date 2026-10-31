// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir %t/main.swift %t/other.swift > /dev/null

// FIXME: This is unsound, we either ought to consistently treat the variables
// as local or global.

//--- main.swift

fn()

let x1 = 0
var x2 = 0
var x3 = 0 { didSet {} }
let x4: Int
var x5: Int

//--- other.swift

func fn() {
  _ = x1
  _ = x2
  _ = x3
  _ = x4
  _ = x5
}
