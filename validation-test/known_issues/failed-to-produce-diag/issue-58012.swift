// rdar://192175680 (https://github.com/swiftlang/swift/issues/58012)
// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype Input
}

struct S<Value>: P {
  typealias Input = Value
}

extension P where Self == S<Int> {
  static var a: Self { .init() }
}

extension P where Self == S<Double> {
  static var a: Self { .init() }
}

protocol View {}

struct Text: View {
  init<F: P>(_ input: F.Input, format: F) {}
}

@resultBuilder
struct Builder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
}

struct V {
  @Builder var body: some View { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    Text(false, format: .a)
  }
}

func test() {
  _ = Text(false, format: .a) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
