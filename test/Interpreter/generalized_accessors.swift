// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

private var backingX = 0
var x: Int {
  _read {
    print("read: yielding \(backingX).")
    yield backingX
    backingX += 1
  }
  _modify {
    var temp = backingX
    print("modify: current value is \(temp).")
    yield &temp
    print("modify: incoming value is \(temp).")
    if temp >= backingX {
      print("modify: accepting.")
      backingX = temp
    } else {
      print("modify: rejecting.")
    }
    print("modify: resulting value is \(backingX).")
  }
}

// CHECK:      0: begin.
print("0: begin.")
// CHECK-NEXT: read: yielding 0.
// CHECK-NEXT: 1: current value of x is 0.
print("1: current value of x is \(x).")
// CHECK-NEXT: read: yielding 1.
// CHECK-NEXT: 2: current value of x is 1.
print("2: current value of x is \(x).")
// CHECK-NEXT: modify: current value is 2.
// CHECK-NEXT: modify: incoming value is -8.
// CHECK-NEXT: modify: rejecting.
// CHECK-NEXT: modify: resulting value is 2.
x -= 10
// CHECK-NEXT: read: yielding 2.
// CHECK-NEXT: 3: current value of x is 2.
print("3: current value of x is \(x).")
// CHECK-NEXT: modify: current value is 3.
// CHECK-NEXT: modify: incoming value is 13.
// CHECK-NEXT: modify: accepting.
// CHECK-NEXT: modify: resulting value is 13.
x += 10
// CHECK-NEXT: read: yielding 13.
// CHECK-NEXT: 4: current value of x is 13.
print("4: current value of x is \(x).")
// CHECK-NEXT: 5: end.
print("5: end.")
