// rdar://140167244
// RUN: %target-typecheck-verify-swift

protocol View {}

@resultBuilder
struct ViewBuilder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
  static func buildEither<T: View>(first: T) -> T { first }
  static func buildEither<T: View>(second: T) -> T { second }
}

struct Binding<Value> {}

struct List<SelectionValue: Hashable, Content: View>: View {
  init(selection: Binding<Set<SelectionValue>>?, @ViewBuilder content: () -> Content) {}
  init(selection: Binding<SelectionValue?>?, @ViewBuilder content: () -> Content) {}
}

struct Major: Hashable {}
struct Minor: Hashable {}

enum CardType: Hashable {
  case majorType(Major)
  case minorType(Minor)
}

struct ShowMajor: View {
  init(major: Major) {}
}

struct ShowMinor: View {
  init(minor: Minor) {}
}

func test(allCards: [CardType]) {
  _ = List(selection: allCards) { card in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    {
      switch card {
      case .majorType(let major):
        ShowMajor(major: major)
      case .minorType(let minor):
        ShowMinor(minor: minor)
      }
    }
  }
}
