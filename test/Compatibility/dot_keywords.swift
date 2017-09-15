// RUN: %target-typecheck-verify-swift -swift-version 3

// Static function in protocol should have `Self.` instead of its protocol name
protocol P {}

extension P {
  static func f1() {}

  func g() {
    f1() // expected-error {{use of unresolved identifier 'f1'}}
  }
}

