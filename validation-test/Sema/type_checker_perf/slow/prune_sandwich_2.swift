// RUN: %target-typecheck-verify-swift -solver-scope-threshold=500

func test4() {
  func f() -> Float { fatalError() }
  func f() -> Int32 { fatalError() }

  func g(_: Int32) {}
  func g(_: Double) {}

  g((f() * 0) + (0 * 0) + (0 * 0))
  // expected-error@-1 {{reasonable time}}
}

func test5() {
  func f() -> Int { fatalError() }
  func f() -> Double { fatalError() }

  func g(_: Double) {}
  func g(_: Float) {}

  g((f() * 0) + (0 * 0) + (0 * 0))
  // expected-error@-1 {{reasonable time}}
}

func test6() {
  func f() -> String { fatalError() }
  func f() -> Int32 { fatalError() }

  func g(_: Int32) {}
  func g(_: String) {}

  g((f() * 0) + (0 * 0) + (0 * 0))
  // expected-error@-1 {{reasonable time}}
}
