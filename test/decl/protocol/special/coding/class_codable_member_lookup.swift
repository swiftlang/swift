// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

class SynthesizedSuperclass : Codable {
  let superValue: Double = .pi
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum including a 'superValue' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{3-6=var}}
}

// Classes which subclass something Codable should be able to override their
// superclasses methods (the methods should be visible in member lookup despite
// being synthesized).
class ExplicitSubclass : SynthesizedSuperclass {
  required init(from decoder: Decoder) throws {
    try super.init(from: decoder)
  }

  override func encode(to encoder: Encoder) throws {
    try super.encode(to: encoder)
  }
}
