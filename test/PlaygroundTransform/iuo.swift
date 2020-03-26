// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -force-single-frontend-invocation -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -playground -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift

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
