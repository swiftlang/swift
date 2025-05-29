// RUN: %batch-code-completion

protocol View {}

struct EmptyView: View {}
struct Either<T: View, U: View>: View {}
struct Tuple<T>: View {}
extension Optional: View where Wrapped: View {}

@resultBuilder
struct ViewBuilder {
  static func buildBlock() -> EmptyView { .init() }
  static func buildBlock<T: View>(_ x: T) -> T { x }
  @_disfavoredOverload static func buildBlock<each T: View>(
    _ x: repeat each T
  ) -> Tuple<(repeat each T)> { .init() }
  static func buildEither<T, U>(first component: T) -> Either<T, U> { .init() }
  static func buildEither<T, U>(second component: U) -> Either<T, U> { .init() }
  static func buildIf<T: View>(_ x: T?) -> T? { x }
}

struct R {
  var bar: Bool
}

func baz<R>(@ViewBuilder _ fn: () -> R) {}

// https://github.com/swiftlang/swift/issues/79213 - Make sure we get a result here.
func foo(_ x: R, _ b: Bool) {
  baz {
    if b {
      EmptyView()
    } else if b {
      if x.#^COMPLETE^# {}
      // COMPLETE: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: bar[#Bool#]; name=bar
    }
  }
}
