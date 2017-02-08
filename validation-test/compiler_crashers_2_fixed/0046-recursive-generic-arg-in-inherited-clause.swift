// RUN: not %target-swift-frontend %s -emit-ir

// https://bugs.swift.org/browse/SR-3354
class A {
  class T {
    class b: Array<T> {

    }
  }
}
