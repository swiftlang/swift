// RUN: %target-swift-frontend -typecheck %s -verify

// https://github.com/apple/swift/issues/55435

class ContainerTransition {
  var viewControllers: [Int: String]?
  func completeTransition() {
    viewControllers?[Int//.max
    // expected-error@-1 {{cannot convert value of type 'Int.Type' to expected argument type 'Int'}}
    // expected-note@-2 {{to match this opening '['}}
  } // expected-error {{expected ']' in expression list}}
}
