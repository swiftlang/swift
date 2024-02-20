import WithMacroDefaultArg
import WithMacroDefaultArgInterface

@freestanding(expression)
macro MagicLine() -> Int = #externalMacro(
    module: "MacroDefinition", type: "MagicLineMacro"
)

@freestanding(expression)
macro Column<T : ExpressibleByIntegerLiteral>() -> T = #externalMacro(
    module: "MacroDefinition", type: "NativeColumnMacro"
)

@freestanding(expression)
macro FilePath<T: ExpressibleByStringLiteral>() -> T = #externalMacro(
    module: "MacroDefinition", type: "NativeFilePathMacro"
)

func printAnotherFileName(file: String = (#FileID)) {
    print(file)
}

func printCurrentFileDefinedAtAnotherFile(file: String = #FileID) {
    print(file)
}

func printCurrentLineDefinedAtAnotherFile(line: Int = #Line) {
    print(line)
}
