// RUN: %swift-ide-test --code-completion --source-filename %s --code-completion-token=COMPLETE

struct ItemDetailView {
  private var itemViewModel: Int
  @ViewBuilder2 var body: some View2 {
    Text2()
      .environmens(\.horizontalSizeClass2, .#^COMPLETE^#regular)
      .onDisappeaq {
        self.itemViewModel
      }
  }
}

protocol View2 {}

extension View2 {
  func onDisappeaq(perform action: (() -> Swift.Void)? = nil) -> some View2 {
    fatalError()
  }
}

@resultBuilder struct ViewBuilder2 {
  static func buildBlock() -> Never { fatalError() }
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
}

enum Foo {
  case regular
}

struct EnvironmentValues2 {
  public var horizontalSizeClass2: Foo
}

public struct Text2 : View2 {
  public init() { fatalError() }
  func environmens<V>(_ keyPath: WritableKeyPath<EnvironmentValues2, V>, _ value: V) -> some View2 { fatalError() }
}
