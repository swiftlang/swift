// RUN: %target-swift-frontend -disable-availability-checking -typecheck -verify %s

// rdar://81531010 - Failure to infer `Context` from contextual return type

protocol View {}
struct EmptyView: View {}

@resultBuilder struct ViewBuilder {
  static func buildBlock() -> EmptyView { EmptyView() }
}

struct Data: View {
}

protocol Modifier {
  associatedtype Body
  associatedtype Content

  @ViewBuilder func body(content: Content) -> Body
}

extension View {
  func config<S: Modifier>(_: S) -> S.Body where S.Content == Self, S.Body: View {
    fatalError()
  }
}

struct DataModifier<Content: View> : Modifier {
  init(_: Data) {}
  func body(content: Content) -> some View {}
}

struct Test {
  typealias Value = DataModifier<Data>.Body

  func test(data: Data) -> Value {
    return data.config(DataModifier(data)) // Ok (Value is not considered an opaque type)
  }
}
