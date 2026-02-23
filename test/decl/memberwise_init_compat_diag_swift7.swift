// RUN: %target-typecheck-verify-swift -swift-version 7 -enable-experimental-feature DeprecateCompatMemberwiseInit

// REQUIRES: swift_feature_DeprecateCompatMemberwiseInit
// REQUIRES: swift7

struct A {
  var a: Int
  private var b = 0

  func foo() {
    _ = Self(a: 0, b: 0) // expected-error {{extra argument 'b' in call}}
  }
}
