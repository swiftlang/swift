// rdar://156936542
// RUN: %target-typecheck-verify-swift

struct Anchor<T> {
  enum Source { case bounds }
}

protocol PreferenceKey {
  associatedtype Value
}

@resultBuilder
struct ViewBuilder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
}

protocol View {
  associatedtype Body: View
  @ViewBuilder var body: Body { get }
}

struct ModifiedView: View {
  var body: Self { self }
}

extension View {
  func id<ID: Hashable>(_ id: ID) -> ModifiedView { fatalError() }

  func anchorPreference<A, K: PreferenceKey>(
    key: K.Type, value: Anchor<A>.Source,
    transform: @escaping (Anchor<A>) -> K.Value
  ) -> ModifiedView { fatalError() }
}

struct Rect {}

struct AnchorsKey: PreferenceKey {
  struct Entry {
    var anchor: Anchor<Rect>
    var kind: Kind

    enum Kind {
      case header
    }
  }
  typealias Value = [AnyHashable: Entry]
}

struct GroupView<ID: Hashable, Header: View>: View {
  let _id: ID
  let header: Header

  var body: some View { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    self.header
      .anchorPreference(key: AnchorsKey.self, value: .bounds) {
        [self.id: .init(anchor: $0, kind: .header)]
      }
  }
}

struct S {
  func id<ID: Hashable>(_ id: ID) {}

  func test() {
    let _: [AnyHashable: Int] = [self.id: 0] // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}
