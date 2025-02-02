// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

func test1(v: Int!) -> [Any]! { nil }
// This is important because it defeats old solver hack that
// checked the number of matching overloads purely based on
// how many parameters there are.
func test1(v: Int!) async throws -> [Int]! { nil }
func test1(v: Int!, other: String = "") throws -> [Int] { [] }

func test2(v: Int!) -> [Any]! { nil }
func test2(v: Int!, other: String = "") throws -> [Int] { [] }

func performTest(v: Int!) {
  guard let _ = test1(v: v) as? [Int] else { // Ok
    return
  }

  guard let _ = test2(v: v) as? [Int] else {
    // expected-error@-1 {{call can throw, but it is not marked with 'try' and the error is not handled}}
    // expected-warning@-2 {{conditional cast from '[Int]' to '[Int]' always succeeds}}
    return
  }
}
