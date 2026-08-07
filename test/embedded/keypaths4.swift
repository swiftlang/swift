// This used to check that key paths were rejected in embedded Swift. They are
// supported now, so the same source has to compile: `\.description` is an
// ordinary computed-property key path on `UInt8`.

// RUN: %target-swift-emit-ir -verify %s -enable-experimental-feature Embedded -wmo | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func foo() {
  let number = 42
  _ = withUnsafeBytes(of: number) { bytes in
      bytes.map(\.description).joined(separator: ".")
  }
}

// CHECK: define {{.*}}@"$e{{.*}}3fooyyF"
