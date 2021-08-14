// RUN: %target-swift-frontend %s -emit-ir -enable-library-evolution -enable-experimental-concurrency
// REQUIRES: concurrency

public protocol P {
  func f() async
}
