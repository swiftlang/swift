// RUN: %batch-code-completion

protocol View {}

extension Never: View {}

@resultBuilder struct ViewBuilder {
  static func buildBlock() -> Never { fatalError() }
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View { fatalError() }
}

struct AsyncImage<Content> : View where Content : View {
  init(@ViewBuilder content: @escaping (Image) -> Content) { fatalError() }
}

struct Image: View {}

extension View {
  func bspectRatio(foo: Int) -> Never { fatalError() }
}

struct ContentView: View {
  var body: some View {
    AsyncImage() { image in
      image.bspectRatio(#^COMPLETE^#foo: 1)
    }
  }
}

// COMPLETE: Decl[InstanceMethod]/Super/Flair[ArgLabels]: ['(']{#foo: Int#}[')'][#Never#]; name=foo:
