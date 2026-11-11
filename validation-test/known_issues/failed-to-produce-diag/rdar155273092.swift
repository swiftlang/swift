// rdar://155273092
// RUN: %target-typecheck-verify-swift

struct Text {
  init(_: String) {}
}

struct _ConditionalContent<T, F> {}

@resultBuilder struct ViewBuilder {
  static func buildBlock<C>(_ c: C) -> C { c }
  static func buildEither<T, F>(first: T) -> _ConditionalContent<T, F> { fatalError() }
  static func buildEither<T, F>(second: F) -> _ConditionalContent<T, F> { fatalError() }
}

protocol TableColumnContent {
  associatedtype TableRowValue
}

struct TableColumn<RowValue, Content>: TableColumnContent {
  typealias TableRowValue = RowValue

  init(_ title: String, @ViewBuilder content: @escaping (RowValue) -> Content) {}
}

@resultBuilder struct TableColumnBuilder<RowValue> {
  static func buildExpression<Content>(_ column: TableColumn<RowValue, Content>) -> TableColumn<RowValue, Content> { column }
  static func buildExpression<Column: TableColumnContent>(_ column: Column) -> Column where Column.TableRowValue == RowValue { column }
  static func buildBlock<Column: TableColumnContent>(_ column: Column) -> Column where Column.TableRowValue == RowValue { column }
}

struct Table<Value, Columns: TableColumnContent> where Value == Columns.TableRowValue {
  init<Data: RandomAccessCollection>(_ data: Data, @TableColumnBuilder<Value> columns: () -> Columns) where Value == Data.Element {}
}

struct Item {
  var comments: [String]
}

func test(items: [Item]) {
  _ = Table(items) { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    TableColumn("Data") { (item: Item) in
      if item.comments != "" {
        Text("A")
      } else {
        Text("B")
      }
    }
  }
}
