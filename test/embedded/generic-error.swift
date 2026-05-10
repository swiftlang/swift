// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -swift-version 5 -wmo -o /dev/null

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// We don't support calling a default witness method (which is generic) with dynamic self.

public protocol P {
  init(x: Int)
}

extension P {
  init(y: Int) {
    self.init(x: y)
  }
}

public class C: P {
  public required init(x: Int) {}

  public convenience init(z: Int) {
    self.init(y: z)  // expected-error {{cannot call an initializer or static method, which is defined as default protocol method, from a class method or initializer}}
  }
}
