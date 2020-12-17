// RUN: %target-swift-frontend %s -emit-ir -enable-experimental-concurrency

protocol P {
  func f<T>() async throws -> T?
}
extension P {
  func f<T>() async throws -> T? { nil }
}
class X: P {}

