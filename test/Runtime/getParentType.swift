// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let suite = TestSuite("GetParentType")

@_silgen_name("_swift_getParentType")
func getParent(of: Any.Type) -> Any.Type?

struct A {
  struct B {}
}

extension Int {
  struct C {}
}

struct D {
  func e() {
    struct F {}

    expectTrue(D.self == getParent(of: F.self))
  }
}

class G {
  enum H {}
}

suite.test("basic") {
  expectTrue(A.self == getParent(of: A.B.self))
  expectTrue(Int.self == getParent(of: Int.C.self))
  D().e()
  expectTrue(G.self == getParent(of: G.H.self))
}

runAllTests()
