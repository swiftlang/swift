// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

guard let x = Optional(42) else { fatalError() }

_ = 0 // intervening code

func function() {
  print("function: \(x)")
}

let closure: () -> Void = {
  print("closure: \(x)")
}

defer {
  print("deferred: \(x)")
}

let closureCapture: () -> Void = { [x] in
  // Must come after the assignment because of the capture by value.
  print("closure-capture: \(x)")
}

print("go! \(x)") // CHECK-LABEL: go! 42
function() // CHECK-NEXT: function: 42
closure() // CHECK-NEXT: closure: 42
closureCapture() // CHECK-NEXT: closure-capture: 42
print("done?") // CHECK-NEXT: done?
// CHECK-NEXT: deferred: 42
