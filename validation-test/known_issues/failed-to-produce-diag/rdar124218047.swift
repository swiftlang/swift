// rdar://124218047
// RUN: %target-typecheck-verify-swift

protocol View {}

@resultBuilder struct ViewBuilder {
  static func buildExpression<C: View>(_ c: C) -> C { c }
  static func buildBlock<C: View>(_ c: C) -> C { c }
}

struct Spacer: View {}

@propertyWrapper struct State<Value> {
  init(wrappedValue: Value) {}
  var wrappedValue: Value { get { fatalError() } nonmutating set {} }
}

protocol ToolbarContent {}

@resultBuilder struct ToolbarContentBuilder {
  static func buildExpression<C: ToolbarContent>(_ c: C) -> C { c }
  static func buildBlock<C: ToolbarContent>(_ c: C) -> C { c }
}

struct ModifiedContent: View {}

extension View {
  func toolbar<C: View>(@ViewBuilder content: () -> C) -> ModifiedContent { fatalError() }
  func toolbar<C: ToolbarContent>(@ToolbarContentBuilder content: () -> C) -> ModifiedContent { fatalError() }
}

struct ContentView: View {
  var body: some View {
    Spacer()
      .toolbar { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
        @State var a: Int =
        Spacer()
      }
  }
}
