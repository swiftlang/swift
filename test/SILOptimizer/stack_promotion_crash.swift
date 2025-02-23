// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// Check that the compiled code does not crash because of a wrong
// stack-promoted array.
// End-to-end test for https://github.com/apple/swift/issues/52844.

public struct Beta {
  var gamma: [Int]
}

class Delta {
  var epsilon: Beta? = Beta(gamma: [])

  func main() {
    for _ in 1...100 {
      crash()
    }
  }

  func crash() {
    epsilon?.gamma = [0]
  }
}

func testit() {
  Delta().main()
}

testit()

// CHECK: ok
print("ok")

