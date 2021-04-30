// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency

func test() async {
  spawn let x = 1 // okay
  _ = await x
}

struct X {
  spawn let x = 1 // expected-error{{'spawn let' can only be used on local declarations}}
  // FIXME: expected-error@-1{{'async' call cannot occur in a property initializer}}
}

func testAsyncFunc() async {
  spawn let (z1, z2) = (2, 3)
  spawn let (_, _) = (2, 3)
  spawn let x2 = 1

  spawn var x = 17 // expected-error{{'spawn' can only be used with 'let' declarations}}{{9-12=let}}
  spawn let (_, _) = (1, 2), y2 = 7 // expected-error{{'spawn let' requires at least one named variable}}
  spawn let y: Int // expected-error{{'spawn let' binding requires an initializer expression}}
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
  spawn let veggies = try await chopVegetables(), meat = await marinateMeat()
  _ = try await veggies
  _ = await meat
}
