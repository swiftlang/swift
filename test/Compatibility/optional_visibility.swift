// RUN: %target-typecheck-verify-swift -enable-objc-interop -swift-version 4

@objc protocol Opt {
  @objc optional func f(callback: @escaping () -> ())
}

class Conforms : Opt {
  private func f(callback: () -> ()) {} // expected-note {{'f' declared here}}
}

func g(x: Conforms) {
  _ = x.f(callback: {}) // expected-error {{'f' is inaccessible due to 'private' protection level}}
}
