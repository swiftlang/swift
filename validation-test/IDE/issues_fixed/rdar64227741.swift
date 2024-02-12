// RUN: %batch-code-completion

struct Earthquake {
  var magnitude: Float
}

@resultBuilder
struct TestBuilder<Q> {
  static func buildBlock(_ exp: String) -> String { "test" }
  static func buildExpression<T>(_ sample: KeyPath<Q, T>) -> String { "test" }
  static func buildExpression(_ func: (Q) -> Bool) -> String { "test" }
}

func ==<V, T: Equatable> (left: KeyPath<V, T>, right: T) -> (V) -> Bool {
  return { $0[keyPath: left] == right }
}

func ||<T> (left: @escaping (T) -> Bool, right: @escaping (T) -> Bool) -> (T) -> Bool {
  return { left($0) || right($0) }
}

class Test<Q> {
  init(@TestBuilder<Q> _ builder: () -> String) {}
}

func test() {
  _ = Test<Earthquake> {
    \.magnitude == 2 || \.#^COMPLETE_1?check=CHECK^#
  }
  _ = Test<Earthquake> {
    \.magnitude == 2 || \.#^COMPLETE_2?check=CHECK^# == 3
  }
}

// CHECK: Begin completions, 1 items
// CHECK: Decl[InstanceVar]/CurrNominal:      magnitude[#Float#]; name=magnitude
