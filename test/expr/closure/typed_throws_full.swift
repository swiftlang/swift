// RUN: %target-typecheck-verify-swift -enable-experimental-feature TypedThrows -enable-upcoming-feature FullTypedThrows

enum MyError: Error {
case failed
case epicFailed
}

func doSomething() throws(MyError) -> Int { 5 }

func apply<T, E: Error>(body: () throws(E) -> T) throws(E) -> T {
  return try body()
}

func doNothing() { }

func testSingleStatement() {
  let c1 = {
    throw MyError.failed
  }
  let _: () throws(MyError) -> Void = c1

  let c2 = {
    try doSomething()
  }
  let _: () throws(MyError) -> Int = c2

  let c3 = {
    return try doSomething()
  }
  let _: () throws(MyError) -> Int = c3
}

func testMultiStatement() {
  let c1 = {
    doNothing()
    throw MyError.failed
  }
  let _: () throws(MyError) -> Void = c1

  let c2 = {
    doNothing()
    return try doSomething()
  }
  let _: () throws(MyError) -> Int = c2
}
