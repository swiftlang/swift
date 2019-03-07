// RUN: %target-typecheck-verify-swift -enable-objc-interop -swift-version 5

@objc protocol Opt {
  @objc optional func f(callback: @escaping () -> ())
}

class Conforms : Opt {
  private func f(callback: () -> ()) {}
  // expected-error@-1 {{method 'f(callback:)' must be declared internal because it matches a requirement in internal protocol 'Opt'}}
  // expected-note@-2 {{mark the instance method as 'internal' to satisfy the requirement}}
}

func g(x: Conforms) {
  _ = x.f(callback: {})
}
