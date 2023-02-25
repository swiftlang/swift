// RUN: %target-run-simple-swift(-enable-experimental-feature VariadicGenerics) | %FileCheck %s

// REQUIRES: executable_test

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

struct G<T...> {
  func makeTuple() {
    print((repeat (Array<each T>)).self)
  }
}

// CHECK: (Array<Int>, Array<String>, Array<Float>)
G<Int, String, Float>().makeTuple()
