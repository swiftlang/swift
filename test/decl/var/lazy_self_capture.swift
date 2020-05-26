// RUN: %target-typecheck-verify-swift -parse-as-library

// This test case must be in a file with no other errors, otherwise we won't
// compute caputures.

class C {
  lazy var foo: String = {
    return self.x
  }()

  let x = "hi"
}
