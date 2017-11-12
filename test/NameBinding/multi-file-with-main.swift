// RUN: %target-swift-frontend -typecheck -enable-source-import -primary-file %s %S/Inputs/multi-file-with-main/main.swift -module-name=MultiFile -sdk "" -verify

func testOperator() {
  let x: Int = 1 +++ "abc" // expected-error {{binary operator '+++' cannot be applied to operands of type 'Int' and 'String'}} expected-note {{expected an argument list of type '(Int, Int)'}}

  _ = x
}
