// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen -parse-as-library -enable-experimental-subclass-existentials %s | %FileCheck %s

protocol Q {}

class Base<T> : Q {
  func classSelfReturn() -> Self {}
  static func classSelfReturn() -> Self {}
}

protocol P {
  func protocolSelfReturn() -> Self
  static func protocolSelfReturn() -> Self
}

class Derived : Base<Int>, P {
  func protocolSelfReturn() -> Self {}
  static func protocolSelfReturn() -> Self {}
}

protocol R {}

// CHECK-LABEL: sil hidden @_T021subclass_existentials11conversionsyAA1P_AA4BaseCySiGXE8baseAndP_AA7DerivedC7derivedAA1R_AIXE0hF1RAaC_AFXEXp0eF5PTypeAIm0H4TypeAaK_AIXEXp0hF5RTypetF : $@convention(thin) (@owned Base<Int> & P, @owned Derived, @owned Derived & R, @thick (Base<Int> & P).Type, @thick Derived.Type, @thick (Derived & R).Type) -> () {

func conversions(
  baseAndP: Base<Int> & P,
  derived: Derived,
  derivedAndR: Derived & R,

  baseAndPType: (Base<Int> & P).Type,
  derivedType: Derived.Type,
  derivedAndRType: (Derived & R).Type) {

  // Values
  let _: Base<Int> = baseAndP
  let _: P = baseAndP
  let _: Q = baseAndP

  let _: Base<Int> & P = derivedAndR

  // Metatypes
  let _: Base<Int>.Type = baseAndPType
  let _: P.Type = baseAndPType
  let _: Q.Type = baseAndPType

  let _: (Base<Int> & P).Type = derivedAndRType
}

func methodCalls(
  baseAndP: Base<Int> & P,
  baseAndPType: (Base<Int> & P).Type) {

  let _: Base<Int> & P = baseAndP.classSelfReturn()
  let _: Base<Int> & P = baseAndP.protocolSelfReturn()

  let _: Base<Int> & P = baseAndPType.classSelfReturn()
  let _: Base<Int> & P = baseAndPType.protocolSelfReturn()

  // Partial applications
  let _: () -> (Base<Int> & P) = baseAndP.classSelfReturn
  let _: () -> (Base<Int> & P) = baseAndP.protocolSelfReturn

  let _: () -> (Base<Int> & P) = baseAndPType.classSelfReturn
  let _: () -> (Base<Int> & P) = baseAndPType.protocolSelfReturn
}

func functionConversions(
  returnsBaseAndP: @escaping () -> (Base<Int> & P),
  returnsBaseAndPType: @escaping () -> (Base<Int> & P).Type,
  returnsDerived: @escaping () -> Derived,
  returnsDerivedType: @escaping () -> Derived.Type,
  returnsDerivedAndR: @escaping () -> Derived & R,
  returnsDerivedAndRType: @escaping () -> (Derived & R).Type) {

  let _: () -> Base<Int> = returnsBaseAndP
  let _: () -> Base<Int>.Type = returnsBaseAndPType

  let _: () -> P = returnsBaseAndP
  let _: () -> P.Type = returnsBaseAndPType

  let _: () -> (Base<Int> & P) = returnsDerived
  let _: () -> (Base<Int> & P).Type = returnsDerivedType

  let _: () -> Base<Int> = returnsDerivedAndR
  let _: () -> Base<Int>.Type = returnsDerivedAndRType

  let _: () -> (Base<Int> & P) = returnsDerivedAndR
  let _: () -> (Base<Int> & P).Type = returnsDerivedAndRType

  let _: () -> P = returnsDerivedAndR
  let _: () -> P.Type = returnsDerivedAndRType
}
