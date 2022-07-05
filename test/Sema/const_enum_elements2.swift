// RUN: %target-typecheck-verify-swift

enum E {
  case a
  case b(Int)

  var c: E { .b(42) }
}

func test(_: _const E) {}

test(.a.c) // expected-error {{expect a compile-time constant literal}}
