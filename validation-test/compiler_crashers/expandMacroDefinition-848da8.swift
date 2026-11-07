// {"kind":"typecheck","signature":"expandMacroDefinition(swift::ExpandedMacroDefinition, swift::MacroDecl*, swift::SubstitutionMap, swift::ArgumentList*)","signatureNext":"evaluateFreestandingMacro"}
// ASTGen builds a different AST for this input and does not reach the crash.
// Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a()
@expression macro b<c, d>() =
  #a<c, d>()
#b<String, Int>
