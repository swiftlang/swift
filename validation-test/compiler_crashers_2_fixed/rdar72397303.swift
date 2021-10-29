// RUN: %target-swift-frontend %s -emit-ir -enable-experimental-concurrency
// REQUIRES: concurrency

@available(SwiftStdlib 5.5, *)
protocol P {
  func f<T>() async throws -> T?
}
@available(SwiftStdlib 5.5, *)
extension P {
  func f<T>() async throws -> T? { nil }
}
@available(SwiftStdlib 5.5, *)
class X: P {}

