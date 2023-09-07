// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/bin 
// RUN: %target-run %t/bin 2> %t/out.txt || true
// RUN: %FileCheck %s < %t/out.txt

struct Example: ~Copyable {
    private var failureString: String { "Goodbye." }
    deinit { fatalError("FATAL ERROR: \(failureString)") }
}

func doit() {
  let e = Example()
  // CHECK: FATAL ERROR: Goodbye.
}

doit()
