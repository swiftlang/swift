// RUN: %target-swift-emit-ir -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func foo() {
  let number = 42
  _ = withUnsafeBytes(of: number) { bytes in
      bytes.map(\.description).joined(separator: ".") // expected-error {{cannot use key path in embedded Swift}}
  }
}
