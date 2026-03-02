// RUN: %target-swift-frontend -target %target-swift-5.5-abi-triple -parse-as-library -c -O %s

// REQUIRES: VENDOR=apple

@available(SwiftStdlib 6.1, *)
actor Bar<T> {
  isolated deinit {}
}

@MainActor
final class Foo<T> {
  isolated deinit {}
}
