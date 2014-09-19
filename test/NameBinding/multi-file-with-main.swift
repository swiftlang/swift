// RUN: %swift -parse -enable-source-import -primary-file %s %S/Inputs/multi-file-with-main/main.swift -module-name=MultiFile -sdk "" -verify

func testOperator() {
  // FIXME: Lousy error message.
  let x: Int = 1 +++ "abc" // expected-error {{cannot convert the expression's type '(IntegerLiteralConvertible, StringLiteralConvertible)' to type 'IntegerLiteralConvertible'}}
  println(x)
}
