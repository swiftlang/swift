// rdar://160807961
// RUN: %target-typecheck-verify-swift

protocol View {}
struct ImmersiveView: View {}
struct TupleView<T>: View {}

@resultBuilder
struct ViewBuilder {
  static func buildBlock<A: View, B: View>(_ a: A, _ b: B) -> TupleView<(A, B)> { TupleView() }
}

protocol CompositorContent {}

@resultBuilder
struct CompositorContentBuilder {
  static func buildBlock<C: CompositorContent>(_ content: C) -> C { content }
  static func buildExpression<C: CompositorContent>(_ content: C) -> C { content }
}

struct CompositorLayer: CompositorContent {}

struct ImmersiveSpace {
  init<V: View>(@ViewBuilder content: () -> V) {}
  init<C: CompositorContent>(@CompositorContentBuilder content: () -> C) {}
}

func test() {
  _ = ImmersiveSpace { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    ImmersiveView()
    CompositorLayer()
  }
}
