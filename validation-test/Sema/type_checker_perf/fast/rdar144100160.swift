// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100
// REQUIRES: asserts,no_asan

typealias TimeInterval = Double

struct Date {
  func addingTimeInterval(_: TimeInterval) -> Date { Date() }
}

func test(date: Date) {
  _ = date.addingTimeInterval(TimeInterval(60 * 60 * 24 * 6 + 12 * 60 + 12 + 1))
}
