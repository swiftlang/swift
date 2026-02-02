// RUN: %target-swift-frontend %s -emit-ir -enable-library-evolution -enable-experimental-concurrency
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
public protocol P {
  func f() async
}
