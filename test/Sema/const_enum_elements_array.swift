// RUN: %target-typecheck-verify-swift

enum E {
  case a
  case b

  var z: E { .b }
}

func getAE() -> E { return .a }

func test_without_const(_ : [E]) {}
func testArr(_ : _const [E]) {}

testArr([])
testArr([.a])
testArr([.a, .b])

testArr([getAE()]) // expected-error {{expect a compile-time constant literal}}
testArr([.a, .b, .a.z]) // expected-error {{expect a compile-time constant literal}}
