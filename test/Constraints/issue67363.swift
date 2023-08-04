// RUN: %target-typecheck-verify-swift -disable-availability-checking

// https://github.com/apple/swift/issues/67363

protocol UIView {
  init()
}

class UILabel : UIView {
  required init() {}
}

class UIStackView : UIView {
  required init() {}
}

protocol ViewRepresentable {
  associatedtype View: UIView
  func configure(view: View)
}

struct StyledString: ViewRepresentable {
  let content: String
  func configure(view: UILabel) {}
}

class StackViewOne<First: UIView>: UIStackView {
  var first = First()
}

struct Stack {
  struct One<First: ViewRepresentable>: ViewRepresentable {
    let first: First
    func configure(view: StackViewOne<First.View>) {
      first.configure(view: view.first)
    }
  }

  @resultBuilder
  enum Builder {
    static func buildBlock<First: ViewRepresentable>(_ first: First) -> Stack.One<First> {
      Stack.One(first: first)
    }
  }

  static func vertical<StackType: ViewRepresentable>(@Builder build builder: () -> StackType) -> StackType {
    builder()
  }
}

struct ListItem {
  let body: any ViewRepresentable
}

@resultBuilder
enum ListBuilder {
  static func buildExpression<View: ViewRepresentable>(_ expression: View?) -> [ListItem?] {
    [expression.map { .init(body: $0) }]
  }

  static func buildBlock(_ components: [ListItem?]...) -> [ListItem] {
    components.flatMap { $0.compactMap { $0 } }
  }
}

struct WithFooter<T: ViewRepresentable>: ViewRepresentable {
  let body: T
  let footer: () -> [ListItem]
  func configure(view: T.View) {}
}

extension ViewRepresentable {
  func withFooter(@ListBuilder build: @escaping () -> [ListItem]) -> WithFooter<Self> {
    .init(body: self, footer: build)
  }
}

func testThatResultBuilderIsAppliedToWithFooterArgument() -> some ViewRepresentable {
  Stack.vertical() {
    StyledString(content: "vertical")
  }
  .withFooter {
    StyledString(content: "footer")
  }
}
