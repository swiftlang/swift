// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/57041

public protocol View2 {
    associatedtype Body : View2
    @ViewBuilder2 var body: Self.Body { get }
}

extension View2 {
  @inlinable public func frame(width: Int? = nil, height: Int? = nil) -> Never { fatalError() }

}

extension Never : View2 {}

extension Optional : View2 where Wrapped: View2 {
  public var body: Never {
    fatalError()
  }
}

@resultBuilder public struct ViewBuilder2 {
  public static func buildBlock() -> Never { fatalError() }
  public static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
  public static func buildIf<Content>(_ content: Content?) -> Content? where Content : View2 { fatalError() }
  public static func buildBlock<C0, C1>(_ c0: C0, _ c1: C1) -> Never where C0 : View2, C1 : View2 { fatalError() }
}

public enum OutlineMenu2: Int {
  case popular
}

public struct OutlineRow : View2 {
  let item: OutlineMenu2

  public var body: some View2 {
    fatalError()
  }
}


public struct ForEach2<Content> {
  public var data: [OutlineMenu2]

  public var content: (OutlineMenu2) -> Content
}

extension ForEach2 : View2 {
  public var body: Never {
    fatalError()
  }
  public typealias Body = Never
}

extension ForEach2 where Content : View2 {
  public init(_ data: [OutlineMenu2], @ViewBuilder2 content: @escaping (OutlineMenu2) -> Content) {
    fatalError()
  }
}

struct SplitView: View2 {
  var body: some View2 {
    ForEach2([OutlineMenu2.popular]) { menu in
      OutlineRow(item: menu)
        .#^COMPLETE^#frame(height: 50)
      if true {
      }
    }
  }
}

// COMPLETE-DAG: Decl[InstanceMethod]/Super/TypeRelation[Convertible]: frame()[#Never#]; name=frame()
// COMPLETE-DAG: Decl[InstanceMethod]/Super/TypeRelation[Convertible]: frame({#width: Int?#}, {#height: Int?#})[#Never#]; name=frame(width:height:)
