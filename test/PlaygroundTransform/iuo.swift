// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground)
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground)

// REQUIRES: executable_test

import PlaygroundSupport

class Layer {
  var value: Double = 0
}

class Outer {
  let layer: Layer! = Layer()
}

class Enclosing {
  var outer: Outer! = Outer()

  func test() {
    // Ensure that this doesn't crash
    outer.layer.value = 3.14159
  }
}

Enclosing().test()
