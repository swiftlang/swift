// RUN: %batch-code-completion

enum E {
  case foo, bar
}
let _: E = .#^COMPLETE^#

// COMPLETE: Begin completions, 3 items
// COMPLETE-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: foo[#E#]; name=foo
// COMPLETE-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: bar[#E#]; name=bar
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): E#})[#(into: inout Hasher) -> Void#]; name=hash(:)
