// RUN: %target-typecheck-verify-swift -swift-version 5

protocol P1 {
  static func `init`(_: Int) // expected-note {{protocol requires function 'init' with type '(Int) -> ()'; do you want to add a stub?}}
  // expected-note@-1 {{did you mean 'init'?}}
}

struct S11 : P1 {
  static func `init`(_: Int) {}
}

struct S12 : P1 { // expected-error {{type 'S12' does not conform to protocol 'P1'}}
  init(_: Int) {}
}

protocol P2 {
  init(_: Int) // expected-note {{protocol requires initializer 'init(_:)' with type 'Int'; do you want to add a stub?}}
}

struct S21 : P2 { // expected-error {{type 'S21' does not conform to protocol 'P2'}}
  static func `init`(_: Int) {}
}

struct S22 : P2 {
  init(_: Int) {}
}

struct S3 {
  static func `init`() {}

  init(x: Int) { // expected-note {{'init(x:)' declared here}}
    self.init() // expected-error {{missing argument for parameter 'x' in call}}
  }
}

_ = S11(0) // expected-error {{argument passed to call that takes no arguments}}
_ = S11.init(0) // expected-error {{argument passed to call that takes no arguments}}
_ = S11.`init`(0)

_ = S12(0)
_ = S12.init(0)
_ = S12.`init`(0) // expected-error {{type 'S12' has no member 'init'}}

_ = S21(0) // expected-error {{argument passed to call that takes no arguments}}
_ = S21.init(0) // expected-error {{argument passed to call that takes no arguments}}
_ = S21.`init`(0)

_ = S22(0)
_ = S22.init(0)
_ = S22.`init`(0) // expected-error {{type 'S22' has no member 'init'}}
