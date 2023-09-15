// RUN: %target-swift-frontend -emit-silgen %s -disable-availability-checking
// REQUIRES: concurrency

public func callee() async -> (() -> ()) {
    fatalError()
}

public func caller() async {
  async let future = callee()
  let result = await future
  result()
}
