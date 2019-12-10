// RUN: %target-swift-frontend -emit-sil -verify -module-name main -primary-file %s %S/Inputs/invalid-magic-literals-other.swift

func bar() {
  badMagicLiteral()
  let _: Int = badGenericMagicLiteral()
}
