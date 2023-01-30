// RUN: not %target-swift-frontend %s -emit-ir

// https://github.com/apple/swift/issues/45942

class A {
  class T {
    class b: Array<T> {

    }
  }
}
