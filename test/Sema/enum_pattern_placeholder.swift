// RUN: %target-typecheck-verify-swift

enum E<T> {
  case x
}

func test(_ value: Any) {
  switch value {
  case E<_>.x: // expected-error {{type placeholder not allowed here}}
    print("E.x")
  default:
    print("not E.x")
  }
}
