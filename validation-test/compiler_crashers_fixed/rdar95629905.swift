// RUN: %target-typecheck-verify-swift

@resultBuilder
struct ViewBuilder {
  static func buildBlock(_ x: Int) -> Int { x }
}

func test(_: () -> Void) -> Int {
  return 42
}

struct MyView {
  @ViewBuilder var body: Int {
    test {
      "ab" is Unknown // expected-error{{cannot find type 'Unknown' in scope}}
      print("x")
    }
  }
}
