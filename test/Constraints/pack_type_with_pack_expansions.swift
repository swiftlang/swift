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
