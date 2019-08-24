// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

class SynthesizedSuperclass : Codable {
  let superValue: Double = .pi
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
