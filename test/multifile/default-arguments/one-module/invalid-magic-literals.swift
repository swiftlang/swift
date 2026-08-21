// RUN: %target-swift-frontend -emit-sil -verify -module-name main -primary-file %s %S/Inputs/invalid-magic-literals-other.swift

func bar() {
  badMagicLiteral() // expected-error {{default argument value of type 'Int' cannot be converted to type 'String'}}
  let _: Int = badGenericMagicLiteral() // expected-error {{default argument value of type 'String' cannot be converted to type 'Int'}}
}
