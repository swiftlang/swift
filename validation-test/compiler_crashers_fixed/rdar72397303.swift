// RUN: %target-swift-frontend %s -emit-ir -enable-experimental-concurrency
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
protocol P {
  func f<T>() async throws -> T?
}
@available(SwiftStdlib 5.1, *)
extension P {
  func f<T>() async throws -> T? { nil }
}
@available(SwiftStdlib 5.1, *)
class X: P {}

