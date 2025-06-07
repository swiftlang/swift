// RUN: %target-swift-frontend -emit-silgen %s -target %target-swift-5.1-abi-triple
// REQUIRES: concurrency

public func callee() async -> (() -> ()) {
    fatalError()
}

public func caller() async {
  async let future = callee()
  let result = await future
  result()
}
