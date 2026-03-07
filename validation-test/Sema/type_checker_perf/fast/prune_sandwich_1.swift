// RUN: %target-typecheck-verify-swift -solver-scope-threshold=500

func test1() {
  func f() -> Bool { fatalError() }
  func f() -> Int32 { fatalError() }

  func g(_: Int32) {}
  func g(_: Bool) {}

  g((f() * 0) + (0 * 0) + (0 * 0))
}

func test2() {
  func f() -> Double { fatalError() }
  func f() -> Int32 { fatalError() }

  func g(_: Int32) {}
  func g(_: Float) {}

  g((f() * 0) + (0 * 0) + (0 * 0))
}

func test3() {
  func f() -> Int { fatalError() }
  func f() -> Int32 { fatalError() }

  func g(_: Int32) {}
  func g(_: Float) {}

  g((f() * 0) + (0 * 0) + (0 * 0))
}

