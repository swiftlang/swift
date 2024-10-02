import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct StructMacro: DeclarationMacro {
    public static func expansion(
        of node: some FreestandingMacroExpansionSyntax,
        in context: some MacroExpansionContext) throws -> [DeclSyntax] {
        return [
        """
        struct MacroStruct {
            @_PlaygroundTransformed
            static func f() -> [Int] {
                print(#function)
                return [1, 2, 3, 4]
            }
        }
        """]
    }
}
