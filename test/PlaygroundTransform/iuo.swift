// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -o %t/main5 -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -o %t/main6 -I=%t %t/PlaygroundSupport.o %t/main.swift

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
