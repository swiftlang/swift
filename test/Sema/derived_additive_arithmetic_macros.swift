// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature AdditiveArithmeticDerivedConformances -enable-experimental-feature DeriveConformancesViaMacros -typecheck -verify -verify-ignore-unrelated %s

// RUN: %target-swift-frontend -enable-experimental-feature AdditiveArithmeticDerivedConformances -typecheck -verify -verify-ignore-unrelated %s

// REQUIRES: swift_feature_AdditiveArithmeticDerivedConformances
// REQUIRES: swift_feature_DeriveConformancesViaMacros

struct Empty: AdditiveArithmetic {}

struct Int2: AdditiveArithmetic {
  var a: Int
  var b: Int
}

struct Nested: AdditiveArithmetic {
  var int2: Int2
  var int: Int
}

struct Mixed: AdditiveArithmetic {
  var nested: Nested
  var float: Float
  var uint8: UInt8
}

struct Generic<T: AdditiveArithmetic>: AdditiveArithmetic {
  var x: T
  var y: T
}

struct Outer<T> {
  struct Inner<U, V> {
    struct GenericContextNested: AdditiveArithmetic {
      var nested: Nested
      var float: Float
    }
  }
}

struct Escaped: AdditiveArithmetic {
  var `default`: Int
  var `foo bar`: Double
}

struct WithLet: AdditiveArithmetic {
  let x: Int
  var y: Int
}

struct WithOtherMembers: AdditiveArithmetic {
  var x: Int
  static var scale: Int = 2
  var doubled: Int { x * 2 }
  func f() {}
}

struct InExtension {
  var x: Int
}

extension InExtension: AdditiveArithmetic {}

struct GenericInExtension<T> {
  var x: T
}

extension GenericInExtension: Equatable, AdditiveArithmetic
where T: AdditiveArithmetic {}

func useThem() {
  func use<T: AdditiveArithmetic>(_ x: inout T) {
    let zero = T.zero
    x += x + zero
    x -= x - zero
  }

  var empty = Empty()
  use(&empty)
  var int2 = Int2(a: 1, b: 2)
  use(&int2)
  var nested = Nested(int2: int2, int: 3)
  use(&nested)
  var mixed = Mixed(nested: nested, float: 1, uint8: 2)
  use(&mixed)
  var generic = Generic<Double>(x: 1, y: 2)
  use(&generic)
  var genericContext =
    Outer<Int>.Inner<Int, Int>.GenericContextNested(nested: nested, float: 1)
  use(&genericContext)
  var escaped = Escaped(default: 1, `foo bar`: 2)
  use(&escaped)
  var withLet = WithLet(x: 1, y: 2)
  use(&withLet)
  var withOtherMembers = WithOtherMembers(x: 1)
  use(&withOtherMembers)
  var inExtension = InExtension(x: 1)
  use(&inExtension)
  var genericInExtension = GenericInExtension<Int>(x: 1)
  use(&genericInExtension)

  let _: Int2 = .zero
  let _: Int2 = Int2(a: 1, b: 2) + .zero
  let _: Int2 = Int2(a: 1, b: 2) - .zero
}

struct LetWithInitialValue: AdditiveArithmetic { // expected-error {{type 'LetWithInitialValue' does not conform to protocol 'AdditiveArithmetic'}}
  // expected-note@-1 {{add stubs for conformance}}
  var x: Int
  let y: Int = 0
}

struct NonConformingProperty: AdditiveArithmetic { // expected-error {{type 'NonConformingProperty' does not conform to protocol 'AdditiveArithmetic'}}
  // expected-note@-1 {{add stubs for conformance}}
  var s: String
}

enum NotAStruct: AdditiveArithmetic { // expected-error {{type 'NotAStruct' does not conform to protocol 'AdditiveArithmetic'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
}
