// RUN: not --crash %target-swift-frontend %s -emit-ir
// REQUIRES: asserts

// https://bugs.swift.org/browse/SR-3354
class A {
  class T {
    class b: Array<T> {

    }
  }
}
