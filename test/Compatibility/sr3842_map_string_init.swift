// RUN: %target-typecheck-verify-swift -swift-version 3

// SR-3842
func test(years: [Int]) {
  _ = years.map(String.init)
}
