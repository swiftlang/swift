// rdar://180299386
// RUN: %target-typecheck-verify-swift

protocol View {}

protocol Transferable {}
extension String: Transferable {}
struct URL: Transferable {}

protocol PrimitiveButtonStyle {}
struct BorderedProminentButtonStyle: PrimitiveButtonStyle {}
extension PrimitiveButtonStyle where Self == BorderedProminentButtonStyle {
  static var borderedProminent: Self { fatalError() }
}
extension View {
  func buttonStyle<S: PrimitiveButtonStyle>(_ style: S) -> some View { self }
}

struct Label: View {}

struct ShareLink<Data: RandomAccessCollection, L: View>: View
where Data.Element: Transferable {}
extension ShareLink where Data.Element == URL {
  init(items: Data, label: () -> L) {}
}
extension ShareLink where Data.Element == String {
  init(items: Data, label: () -> L) {}
}

func test(text: String, url: URL) -> some View {
  ShareLink(items: [text, url]) {
    Label()
  }
  .buttonStyle(.borderedProminent) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
