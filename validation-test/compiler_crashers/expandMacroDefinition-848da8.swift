// {"kind":"typecheck","signature":"expandMacroDefinition(swift::ExpandedMacroDefinition, swift::MacroDecl*, swift::SubstitutionMap, swift::ArgumentList*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a()
@expression macro b<c, d>() =
  #a<c, d>()
#b<String, Int>
