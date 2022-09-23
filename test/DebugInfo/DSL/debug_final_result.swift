@resultBuilder
enum SimpleBuilder {
  struct Component<Wrapped> {
    let value: Wrapped
    let debugInfoProvider: DSLDebugInfoProvider?
  }

  static func buildExpression(
    _ expression: Int
  ) -> Component<Int> {
    .init(value: expression, debugInfoProvider: nil)
  }

  static func buildDebuggable(
    component: Component<Int>,
    debugInfoProvider: DSLDebugInfoProvider
  ) -> Component<Int> {
    .init(value: component.value, debugInfoProvider: debugInfoProvider)
  }
  
  static func buildDebuggable(
    finalResult: FinalResult,
    debugInfoProvider: DSLDebugInfoProvider
  ) -> FinalResult {
    .init(value: component.value, debugInfoProvider: debugInfoProvider)
  }

  static func buildBlock(_ components: Component<Int>...) -> [Component<Int>] {
    components
  }
  
  static func buildFinalResult(
    _ component: [Component<Int>]
  ) -> FinalResult {
    .init(value: component, debugInfoProvider: nil)
  }
  
  typealias FinalResult = Component<[Component<Int>]>
}

func build(@SimpleBuilder make: () -> SimpleBuilder.FinalResult)
-> SimpleBuilder.FinalResult {
  return make()
}

let built = build {
  1
  2
}

for each in built.value {
  each.debugInfoProvider?.callAsFunction(context: each.value * 2)
}
built.debugInfoProvider?.callAsFunction(context: "YES")
