// RUN: %target-run-simple-swift | %FileCheck %s

// RUN: %target-build-swift -DVAR %s -o %t-var
// RUN: %target-codesign %t-var
// RUN: %target-run %t-var | %FileCheck %s

// RUN: %target-build-swift -DVAR_UPDATE %s -o %t-var2
// RUN: %target-codesign %t-var2
// RUN: %target-run %t-var2 | %FileCheck %s

// REQUIRES: executable_test

#if VAR_UPDATE
guard var x = Optional(0) else { fatalError() }
#elseif VAR
guard var x = Optional(42) else { fatalError() }
#else
guard let x = Optional(42) else { fatalError() }
#endif

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

#if VAR_UPDATE
x = 42
#endif

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
