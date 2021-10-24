// RUN: %target-swift-frontend %s -emit-ir -enable-library-evolution -enable-experimental-concurrency
// REQUIRES: concurrency

@available(SwiftStdlib 5.5, *)
public protocol P {
  func f() async
}
