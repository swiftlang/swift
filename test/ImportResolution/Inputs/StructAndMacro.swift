public struct Foo {
  public var value: Int
  public init(value: Int) { self.value = value }
}

@freestanding(expression)
public macro Foo() -> Int = #externalMacro(module: "NonExistent", type: "NonExistent")
