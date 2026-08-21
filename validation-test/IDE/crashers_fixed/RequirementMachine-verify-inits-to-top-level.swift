// RUN: %batch-code-completion -code-complete-inits-in-postfix-expr
protocol P {
  init(x: Int)
}
extension P {
  init(x: Int) {}
}
struct S<T, U>: P {}

#^COMPLETE^#
// COMPLETE: Decl[Constructor]/CurrModule: S({#x: Int#})[#S<T, U>#]; name=S(x:)
