// rdar://165865278 (https://github.com/swiftlang/swift/issues/58198)
// RUN: %target-typecheck-verify-swift

protocol View {}

@resultBuilder struct ViewBuilder {
  static func buildExpression<C: View>(_ c: C) -> C { c }
  static func buildBlock<C: View>(_ c: C) -> C { c }
  static func buildBlock<each C: View>(_ c: repeat each C) -> TupleView<(repeat each C)> { fatalError() }
}

struct TupleView<T>: View {}
struct Text: View { init(_ s: String) {} }
struct EditButton: View {}
struct Button<Label: View>: View {
  init(_ title: String, systemImage: String, action: @escaping () -> Void) where Label == Text {}
}

struct NavigationStack<Root: View>: View {
  init(@ViewBuilder root: () -> Root) {}
}
struct List<Content: View>: View {
  init(@ViewBuilder content: () -> Content) {}
}

protocol ToolbarContent {}
protocol CustomizableToolbarContent: ToolbarContent {}

struct ToolbarItemPlacement {
  static var bottomBar: ToolbarItemPlacement { fatalError() }
}

struct ToolbarItem<ID, Content: View>: ToolbarContent {}
extension ToolbarItem: CustomizableToolbarContent where ID == String {}
extension ToolbarItem where ID == () {
  init(placement: ToolbarItemPlacement, @ViewBuilder content: () -> Content) {}
}

struct TupleToolbarContent<T>: ToolbarContent {}

@resultBuilder struct ToolbarContentBuilder {
  static func buildExpression<C: ToolbarContent>(_ c: C) -> C { c }
  static func buildExpression<C: CustomizableToolbarContent>(_ c: C) -> C { c }
  static func buildBlock<C: ToolbarContent>(_ c: C) -> C { c }
  static func buildBlock<C: CustomizableToolbarContent>(_ c: C) -> C { c }
  static func buildBlock<C0: ToolbarContent, C1: ToolbarContent>(_ c0: C0, _ c1: C1) -> TupleToolbarContent<(C0, C1)> { fatalError() }
  static func buildBlock<C0: CustomizableToolbarContent, C1: CustomizableToolbarContent>(_ c0: C0, _ c1: C1) -> TupleToolbarContent<(C0, C1)> { fatalError() }
}

struct ModifiedContent: View {}

extension View {
  func toolbar<C: View>(@ViewBuilder content: () -> C) -> ModifiedContent { fatalError() }
  func toolbar<C: ToolbarContent>(@ToolbarContentBuilder content: () -> C) -> ModifiedContent { fatalError() }
}

struct MyView: View {
  var body: some View {
    NavigationStack { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      List {
        Text("")
      }
      .toolbar {
        Button("Scan", systemImage: "qrcode.viewfinder") {
        }

        ToolbarItem(placement: .bottomBar) {
          EditButton()
        }
      }
    }
  }
}
