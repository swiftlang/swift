// RUN: %target-swift-frontend -c %s -verify

func testFatalError(_ message: @autoclosure () -> String = String()) -> Never {
  Swift.fatalError()
}

func test() {
  testFatalError()
}
