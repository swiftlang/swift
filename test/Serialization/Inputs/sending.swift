
public func testSending(_ x: sending String) -> sending String { x }
// expected-warning @-1 {{'sending' has no effect on Sendable parameter}}
public func testSendingFunc(_ x: (sending String) -> ()) { fatalError() }
// expected-warning @-1 {{'sending' has no effect on Sendable parameter}}
public func testSendingResultFunc(_ x: () -> sending String) { fatalError() }
// expected-warning @-1 {{'sending' has no effect on Sendable parameter}}
