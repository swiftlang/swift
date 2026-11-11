// rdar://155790733
// RUN: %target-typecheck-verify-swift

protocol View {}

@resultBuilder struct ViewBuilder {
  static func buildExpression<C: View>(_ c: C) -> C { c }
  static func buildBlock<C: View>(_ c: C) -> C { c }
  static func buildBlock<C0: View, C1: View>(_ c0: C0, _ c1: C1) -> TupleView<(C0, C1)> { fatalError() }
}

struct TupleView<T>: View {}
struct Text: View { init(_ s: String) {} }
struct Button<Label: View>: View {
  init(_ title: String, action: @escaping () -> Void) where Label == Text {}
}

protocol ToolbarContent {}
struct TupleToolbarContent<T>: ToolbarContent {}

@resultBuilder struct ToolbarContentBuilder {
  static func buildExpression<C: ToolbarContent>(_ c: C) -> C { c }
  static func buildBlock<C: ToolbarContent>(_ c: C) -> C { c }
  static func buildBlock<C0: ToolbarContent, C1: ToolbarContent>(_ c0: C0, _ c1: C1) -> TupleToolbarContent<(C0, C1)> { fatalError() }
}

struct ToolbarItemPlacement {
  static var cancellationAction: Self { fatalError() }
  static var confirmationAction: Self { fatalError() }
}

struct ToolbarItemGroup<Content: View>: ToolbarContent {
  init(placement: ToolbarItemPlacement, @ViewBuilder content: () -> Content) {}
}

struct ModifiedContent: View {}

extension View {
  func toolbar<C: View>(@ViewBuilder content: () -> C) -> ModifiedContent { fatalError() }
  func toolbar<C: ToolbarContent>(@ToolbarContentBuilder content: () -> C) -> ModifiedContent { fatalError() }
}

struct Record {
  var reason: String?
  var date: Int
}

struct RecordFormView: View {
  let onAdd: (Record) -> Void
  func dismiss() {}

  var body: some View {
    Text("")
      .toolbar { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
        ToolbarItemGroup(placement: .cancellationAction) {
          Button("") { dismiss() }
        }
        ToolbarItemGroup(placement: .confirmationAction) {
          Button("") {
            onAdd(Record(reason: "", startDate: 0, endDate: 0))
            dismiss()
          }
        }
      }
  }
}
