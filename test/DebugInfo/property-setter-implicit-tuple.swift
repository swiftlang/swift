// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// This is a test for rdar://125939953 (Implicit variables are removed at Onone)

struct UInt128 {
  var low: UInt64
  var high: UInt64

  var components: (low: UInt64, high: UInt64) {
    get {
      return (low, high)
    }
    // CHECK-LABEL: define {{.+}} @"$s4main7UInt128V10componentss6UInt64V3low_AF4hightvs"
    set {
      // CHECK: #dbg_declare(ptr {{.+}}, ![[NEW_VALUE:[0-9]+]], !DIExpression
      (self.low, self.high) = (newValue.high, newValue.low)
    }
  }
}

//CHECK: ![[NEW_VALUE]] = !DILocalVariable(name: "newValue", arg: 1
