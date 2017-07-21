// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Structs with no initializers should get a memberwise initializer.
struct NoInitializers {
  var x: Double // need not have an initial value

  func foo() {
    // The struct should receive a memberwise initializer.
    let _ = NoInitializers.init(x:)
  }
}

// A similar struct with Codable properties adopting Codable should get a
// synthesized init(from:), along with the memberwise initializer.
struct CodableNoExplicitInitializers : Codable {
  var x: Double

  func foo() {
    // The struct should receive a synthesized init(from:) and encode(to:).
    let _ = CodableNoExplicitInitializers.init(from:)
    let _ = CodableNoExplicitInitializers.encode(to:)

    // It should still receive a memberwise initializer.
    let _ = CodableNoExplicitInitializers.init(x:)
  }
}

// The same should hold for structs whose members all have initial values.
struct InitialValueNoInitializers {
  var x: Double = .pi

  func foo() {
    // The struct should receive a memberwise initializer.
    let _ = InitialValueNoInitializers.init(x:)

    // The struct should receive a no-argument initializer.
    let _ = InitialValueNoInitializers.init()
  }
}

struct InitialValueCodableNoExplicitInitializers : Codable {
  var x: Double = .pi

  func foo() {
    // The struct should receive a synthesized init(from:) and encode(to:).
    let _ = InitialValueCodableNoExplicitInitializers.init(from:)
    let _ = InitialValueCodableNoExplicitInitializers.encode(to:)

    // It should still receive a memberwise initializer.
    let _ = InitialValueCodableNoExplicitInitializers.init(x:)

    // It should still receive a no-argument initializer.
    let _ = InitialValueCodableNoExplicitInitializers.init()
  }
}
