// Check that we are not crashing
// RUN: %sourcekitd-test -req=complete -pos=48:28 %s -- %s == -req=complete -pos=48:16 %s -- %s

let abcde = "abc"

public struct MyEmptyView : MyView {
  public typealias Body = Never
  public var body: Never { fatalError() }
}

extension Never : MyView {}

@resultBuilder public struct MyViewBuilder {
  public static func buildBlock() -> MyEmptyView {
        return MyEmptyView()
    }
  public static func buildBlock<Content>(_ content: Content) -> Content where Content : MyView {
        content
    }
}

public protocol MyView {
  associatedtype Body : MyView
  @MyViewBuilder var body: Self.Body { get }
}

public struct MyHStack<Content> : MyView {
  public init(@MyViewBuilder content: () -> Content) {}
  public typealias Body = Swift.Never
  public var body: Never { fatalError() }
}

public struct MyText : MyView {
  public init(_ content: Swift.String) {}
  public typealias Body = Never
  public var body: Never { fatalError() }
}

extension MyView {
  public func padding(_ insets: Bool) -> some MyView { return MyEmptyView() }
  public func padding(_ length: Float) -> some MyView { return MyEmptyView() }
  public func padding() -> some MyView { return MyEmptyView() }
}

struct RoundedBadge : MyView {
  var body: some MyView {
    MyHStack {
      MyText(abcde).padding()
    }
  }
}
