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
  // expected-note@+1{{candidate expects value of type '[URL]' for parameter #1 (got '[Any]')}}
  init(items: Data, label: () -> L) {}
}
extension ShareLink where Data.Element == String {
  // expected-note@+1{{candidate expects value of type '[String]' for parameter #1 (got '[Any]')}}
  init(items: Data, label: () -> L) {}
}

func test(text: String, url: URL) -> some View {
  // expected-error@+1{{no exact matches in call to initializer}}
  ShareLink(items: [text, url]) {
    Label()
  }
  .buttonStyle(.borderedProminent)
}
