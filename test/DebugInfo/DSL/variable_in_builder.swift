struct A {
  let value: Int
  @SimpleBuilder
  let whatever: () -> [SimpleBuilder.Component]
}

@resultBuilder
enum SimpleBuilder {
  struct Component {
    let value: A
    let debugInfoProvider: DSLDebugInfoProvider?
  }

  static func buildExpression(_ expression: A) -> Component {
    .init(value: expression, debugInfoProvider: nil)
  }

  static func buildBlock(_ components: Component...) -> [Component] {
    return components
  }
  
  // buildDebuggable
}

func build(@SimpleBuilder make: () -> [SimpleBuilder.Component]) -> [SimpleBuilder.Component] {
  return make()
}

let built = build {
  var b = A(value: 2) { }
  A(value: 1) {
    b
  }
}

dump(built[0].value.whatever()[0].value.whatever())

/*
// TEST ALSO NESTED CLOSURE
let built = build {
  A(value: 1) {
    A(value: 2) {

    }
  }
}

dump(built[0].value.whatever()[0].value.whatever())
*/
