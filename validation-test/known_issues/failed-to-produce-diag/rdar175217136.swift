// rdar://175217136
// RUN: %target-typecheck-verify-swift

protocol View {}

@resultBuilder struct ViewBuilder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
}

struct Text: View { init(_ s: String) {} }

extension View {
  func onChange<V: Equatable>(
    of value: V, perform action: @escaping (V) -> Void
  ) -> some View { self }
}

struct MetricCard {
  var animatedValue: Double = 0

  @ViewBuilder var body: some View { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    Text("")
      .onChange(of: animatedValue, perform: haptic)
  }

  func haptic() {}
}
