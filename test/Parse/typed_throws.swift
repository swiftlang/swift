// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name test

// Parsing support for typed throws.

enum MyError: Error {
  case epicFail
}

func specificThrowingFunc() throws(MyError) { }
func anyThrowingFunc() throws(any Error) { }

struct SomeStruct {
  init() throws(MyError) { }
}

typealias ThrowingFuncType = () throws(MyError) -> Void

func testClosures() {
  let _ = { () throws(MyError) in print("hello") }
  let _ = { (x: Int, y: Int) throws(MyError) -> Int in x + y }
}

@available(SwiftStdlib 6.0, *)
func testTypes() {
  let _ = [(Int, Int) throws(MyError) -> Int]()
}

func testMissingError() throws() { }
// expected-error@-1{{expected thrown error type after 'throws('}}

func testRethrowsWithThrownType() rethrows(MyError) { }
// expected-error@-1{{'rethrows' cannot be combined with a specific thrown error type}}

struct S<Element, Failure: Error> {
  init(produce: @escaping () async throws(Failure) -> Element?) {
  }
}
