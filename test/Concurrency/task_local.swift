// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
enum TL {
  @TaskLocal
  @available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
  static var number: Int = 0
}

func test () async {
  TaskLocalValues.number = 10 // expected-error{{setter for 'number' is unavailable}}
}
