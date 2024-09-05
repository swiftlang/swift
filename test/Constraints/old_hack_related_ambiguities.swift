// RUN: %target-typecheck-verify-swift

func entity(_: Int) -> Int {
  0
}

struct Test {
  func test(_ v: Int) -> Int { v }
  func test(_ v: Int?) -> Int? { v }
}

func test_ternary_literal(v: Test) -> Int? {
  true ? v.test(0) : nil // Ok
}

func test_ternary(v: Test) -> Int? {
  true ? v.test(entity(0)) : nil // Ok
}

do {
  struct TestFloat {
    func test(_ v: Float) -> Float { v } // expected-note {{found this candidate}}
    func test(_ v: Float?) -> Float? { v } // expected-note {{found this candidate}}
  }

  func test_ternary_non_default_literal(v: TestFloat) -> Float? {
    true ? v.test(1.0) : nil // expected-error {{ambiguous use of 'test'}}
  }
}

do {
  struct Test {
    init(a: Int, b: Int = 0) throws {}
    init?(a: Int?) {}
  }

  func test(v: Int) -> Test? {
    return Test(a: v) // Ok
  }
}

