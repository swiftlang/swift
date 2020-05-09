// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// A class with no initializers (which has non-initialized properties so a
// default constructor can be synthesized) should produce an error.
class NoInitializers { // expected-error {{class 'NoInitializers' has no initializers}}
  var x: Double // expected-note {{stored property 'x' without initial value prevents synthesized initializers}}

  func foo() {
    // The class should not receive a default constructor.
    let _ = NoInitializers.init() // expected-error {{'NoInitializers' cannot be constructed because it has no accessible initializers}}
  }
}

// A similar class with Codable properties adopting Codable should get a
// synthesized init(from:), and thus not warn.
class CodableNoExplicitInitializers : Codable {
  var x: Double

  func foo() {
    // The class should receive a synthesized init(from:) and encode(to:)
    let _ = CodableNoExplicitInitializers.init(from:)
    let _ = CodableNoExplicitInitializers.encode(to:)

    // It should not, however, receive a default constructor.
    let _ = CodableNoExplicitInitializers.init() // expected-error {{missing argument for parameter 'from' in call}}
  }
}

// A class with all initialized properties should receive a default constructor.
class DefaultConstructed {
  var x: Double = .pi

  func foo() {
    let _ = DefaultConstructed.init()
  }
}

// A class with all initialized, Codable properties adopting Codable should get
// the default constructor, along with a synthesized init(from:).
class CodableDefaultConstructed : Codable {
  var x: Double = .pi

  func foo() {
    let _ = CodableDefaultConstructed.init()
    let _ = CodableDefaultConstructed.init(from:)
    let _ = CodableDefaultConstructed.encode(to:)
  }
}
