// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

// rdar://181061095
//
// Ensure that x is deinitialised at the end of the loop in demo.
//
// In the none case of `var x = orig?.retained()`, x is initialised with
// inject_enum_addr. This fully initialises x, including the payload bits,
// because Optional's none case has no payload. Verify that liveness analysis
// respects this, and does not incorrectly treat x as live at the end of the
// loop body, which previously caused it to remove the deinit call.

struct Holder: ~Copyable {
  let tag: String
  init(_ tag: String) { self.tag = tag }

  borrowing func retained() -> Holder { Holder("retained-\(tag)") }
  mutating func touch() {}

  deinit { print("deinit: \(tag)") }
}

func demo() {
  let orig: Holder? = Holder("original")
  for _ in 0..<2 {
    var x = orig?.retained()
    x!.touch()
    print("end of iteration")
  }
  print("end of loop")
}

//      CHECK: end of iteration
// CHECK-NEXT: deinit: retained-original
// CHECK-NEXT: end of iteration
// CHECK-NEXT: deinit: retained-original
// CHECK-NEXT: end of loop
// CHECK-NEXT: deinit: original
demo()
