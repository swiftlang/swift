struct A {
  let value: Int
}

@resultBuilder
enum SimpleBuilder {
  struct Component {
    let value: Int
    let debugInfoProvider: DSLDebugInfoProvider?
  }

  static func buildExpression(
    _ expression: A
  ) -> Component {
    .init(value: expression.value, debugInfoProvider: nil)
  }

  static func buildBlock(_ components: Component...) -> [Component] {
    return components
  }
}

func build(@SimpleBuilder make: () -> [SimpleBuilder.Component]) -> [SimpleBuilder.Component] {
  return make()
}

let built = build {
  A(value: 1)
}
