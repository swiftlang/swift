// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: executable_test

// XFAIL: swift_test_mode_optimize_armv7

struct Structure {
  var a: UInt8
  var b: UInt8
  var c: UInt8
}

enum Enum: Int {
  case One, Two, Three, Four
}

var x: [Enum: (Structure?, Structure?)] = [
    .One: (Structure(a: 1, b: 2, c: 3), nil)
]

// CHECK: [main.Enum.One: (Optional(main.Structure(a: 1, b: 2, c: 3)), nil)]
print(x)
