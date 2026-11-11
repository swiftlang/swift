// rdar://115513112
// RUN: %target-typecheck-verify-swift

protocol View {}

struct Image: View {}
struct Size {}
enum ContentMode { case fit, fill }

@resultBuilder
struct ViewBuilder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
}

struct GeometryReader<Content: View>: View {
  init(@ViewBuilder content: () -> Content) {}
}

extension View {
  func aspectRatio(_ aspectRatio: Double?, contentMode: ContentMode) -> Image { fatalError() }
  func aspectRatio(_ aspectRatio: Size, contentMode: ContentMode) -> Image { fatalError() }
}

struct V: View {
  var body: some View {
    GeometryReader { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      Image().aspectRatio(aspectRatio, contentMode: .fill)
    }
  }
}
