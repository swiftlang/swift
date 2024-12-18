// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

// rdar://137125054

do {
  struct Tuple<each T> {
    func withBool() -> Tuple<repeat each T, Bool> {}
  }

  let t = Tuple<Int>()
    .withBool()
    .withBool()
    .withBool()
  let _: Tuple<
    Int,
    Bool,
    Bool,
    Bool
  > = t
}

protocol View {}

struct Zero: View {}
struct One: View {}

protocol ViewModifier {}

struct Add: ViewModifier {}
struct Multiply: ViewModifier {}

struct ModifiedContent<Content: View, each Modifier: ViewModifier>: View {
  func add() -> ModifiedContent<Content, repeat each Modifier, Add> {}
  func multiply() -> ModifiedContent<Content, repeat each Modifier, Multiply> {}
}

extension View {
  func add() -> ModifiedContent<Self, Add> {}
  func multiply() -> ModifiedContent<Self, Multiply> {}
}

do {
  let m = Zero()
    .add()
    .multiply()
    .add()
    .multiply()
    .add()
    .multiply()
    .add()
    .multiply()
    .add()
    .multiply()

  let _: ModifiedContent<
    Zero,
    Add,
    Multiply,
    Add,
    Multiply,
    Add,
    Multiply,
    Add,
    Multiply,
    Add,
    Multiply
  > = m
}

// FIXME: The constraint system cannot handle these!
do {
  // expected-note@+2 {{arguments to generic parameter 'T' ('[Int], [Bool]' and 'Array<_>') are expected to be equal}}
  // expected-note@+1 {{arguments to generic parameter 'T' ('[Int], [Bool], [Bool], [String], [Never]' and 'Array<_>, Array<_>, Array<_>') are expected to be equal}}
  struct G<each T> {}

  do {
    func f<each T>(
      _: G<repeat [each T]> // expected-note {{in inferring pack element #0 of '_'}}
    ) -> G<repeat [each T], [Bool]> {}

    let g1: G<[Int], [Bool]>

    // expected-error@+3 {{could not infer pack element #0 from context}}
    // expected-error@+2 {{pack expansion requires that '[Int], [Bool]' and '_' have the same shape}}
    // expected-error@+1 {{cannot convert value of type 'G<[Int], [Bool]>' to expected argument type 'G<Array<_>>'}}
    let _ = f(f(g1))
  }

  do {
    func g<each T, each U>(
      _: G<repeat each U>,
      _: G<repeat each T>
    ) -> G<repeat [each T], [Bool], repeat [each U]> {}
    func f<each T>(
      // expected-note@+3 {{in inferring pack element #2 of '_'}}
      // expected-note@+2 {{in inferring pack element #1 of '_'}}
      // expected-note@+1 {{in inferring pack element #0 of '_'}}
      _: G<repeat [each T]>
    ) -> G<repeat [each T], [Bool]> {}

    let g1: G<String, Never>
    let g2: G<Int, Bool>

    // expected-error@+5 {{cannot convert value of type 'G<[Int], [Bool], [Bool], [String], [Never]>' to expected argument type 'G<Array<_>, Array<_>, Array<_>>'}}
    // expected-error@+4 {{pack expansion requires that '[Int], [Bool], [Bool], [String], [Never]' and '_, _, _' have the same shape}}
    // expected-error@+3 {{could not infer pack element #2 from context}}
    // expected-error@+2 {{could not infer pack element #1 from context}}
    // expected-error@+1 {{could not infer pack element #0 from context}}
    let _ = f(g(g1, g2))
  }
}
