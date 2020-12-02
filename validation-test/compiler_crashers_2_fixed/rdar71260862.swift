// RUN: %target-swift-frontend %s -emit-ir -enable-library-evolution -enable-experimental-concurrency
// REQUIRES: concurrency

public class X {
  public func f() async { }
}
