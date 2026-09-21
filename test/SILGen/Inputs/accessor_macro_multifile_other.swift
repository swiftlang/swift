@attached(accessor, names: named(get))
public macro AddGetter() = #externalMacro(module: "MacroDefinition", type: "AddGetterMacro")

public struct Holder {
  @AddGetter
  var foo: Int = 0
}
