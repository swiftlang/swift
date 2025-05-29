@freestanding(expression)
public macro Line<T : ExpressibleByIntegerLiteral>() -> T = #externalMacro(
    module: "MacroDefinition", type: "NativeLineMacro"
)

public func printCurrentLineDefinedAtAnotherModule(line: Int = #Line) {
    print(line)
}
