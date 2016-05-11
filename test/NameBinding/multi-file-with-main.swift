// RUN: %target-swift-frontend -parse -enable-source-import -primary-file %s %S/Inputs/multi-file-with-main/main.swift -module-name=MultiFile -sdk "" -verify

func testOperator() {
  let x: Int = 1 +++ "abc" // expected-error {{cannot convert value of type 'String' to expected argument type 'Int'}}

  _ = x
}
