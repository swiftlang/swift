// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

func test() async {
  async let x = 1 // okay
  _ = await x
}

struct X {
  async let x = 1 // expected-error{{'async let' can only be used on local declarations}}
}

func testAsyncFunc() async {
  async let (z1, z2) = (2, 3)
  async let (_, _) = (2, 3)
  async let x2 = 1

  async var x = 17 // expected-error{{'async' can only be used with 'let' declarations}}{{9-12=let}}
  async let (_, _) = (1, 2), y2 = 7 // expected-error{{'async let' requires at least one named variable}}
  async let y: Int // expected-error{{'async let' binding requires an initializer expression}}
  _ = await x
  _ = y
  _ = await z1
  _ = await z2
  _ = await x2
  await x = 1
  _ = y2
}

// Cooking example
func chopVegetables() async throws -> [String] { [] }
func marinateMeat() async -> String { "MEAT" }

func cook() async throws {
  async let veggies = try await chopVegetables(), meat = await marinateMeat()
  _ = try await veggies
  _ = await meat
}

func testInterpolation() async {
  async let y = "\(12345)"
  _ = await y
}
