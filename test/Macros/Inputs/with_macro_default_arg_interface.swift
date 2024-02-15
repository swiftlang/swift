@freestanding(expression)
public macro FileID<T: ExpressibleByStringLiteral>() -> T = #externalMacro(
    module: "MacroDefinition", type: "NativeFileIDMacro"
)

@freestanding(expression)
public macro PrependHelloToShadowed() -> String = #externalMacro(
    module: "MacroDefinition", type: "PrependHelloToShadowedMacro"
)

@freestanding(expression)
public macro MakeClosureCaller() -> ClosureCaller = #externalMacro(
    module: "MacroDefinition", type: "ClosureCallerMacro"
)

public func printCurrentFileDefinedInAnotherModuleInterface(
    file: String = #FileID
) {
    print(file)
}

public struct ClosureCaller {
    private let callback: @convention(thin) (Any, () -> Void) -> Void

    public init(_ callback: @convention(thin) (Any, () -> Void) -> Void) {
        self.callback = callback
    }

    public func callAsFunction(context: Any, then: () -> Void = {}) {
        callback(context, then)
    }
}

public let shadowed = "world"

public func preferVariableFromLocalScope(
    param: String = #PrependHelloToShadowed
) {
    print(param)
}

@resultBuilder
public enum ClosureCallerBuilder {
    public static func buildBlock(
        closureCaller: ClosureCaller = #MakeClosureCaller
    ) -> ClosureCaller {
        closureCaller
    }
}
