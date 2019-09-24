// RUN: %target-swift-frontend -typecheck -enable-source-import -primary-file %s %S/Inputs/multi-file-with-main/main.swift -module-name=MultiFile -sdk "" -verify

func testOperator() {
  let x: Int = 1 +++ "abc" // expected-error@:22 {{cannot convert value of type 'String' to expected argument type 'Int'}}

  _ = x
}
