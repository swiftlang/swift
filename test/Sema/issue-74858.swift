// RUN: %target-typecheck-verify-swift

typealias Alias<T> = Int

func invalidSpecializeExpr(_ x: Alias<Int>.Type) {
  let y = x<Int>.self
  // expected-error@-1 {{type of expression is ambiguous without a type annotation}}
  // FIXME: Bad diagnostic
}
