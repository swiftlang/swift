// RUN: %target-typecheck-verify-swift

// rdar://114402042 - Make sure we connect the SingleValueStmtExpr to the outer
// closure's return type.
func foo<T>(_: () -> T) {}
func bar<T>(_ x: T) {}
func test() {
  foo {
    bar(if true { return } else { return })
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    // expected-error@-2 2{{cannot 'return' in 'if' when used as expression}}
  }
  foo {
    bar(if true { { return } } else { { return } })
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
  }
}

func baz() -> String? {
  nil
}

var x: Int? = {
  print( // expected-error {{cannot convert value of type '()' to closure result type 'Int?'}}
    // expected-note@-1 {{to match this opening '('}}
    switch baz() {
    case ""?:
        return nil
    default:
        return nil
    }
}() // expected-error {{expected ')' in expression list}}
