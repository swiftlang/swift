// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -parse -verify -disable-objc-attr-requires-foundation-module %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPTIONAL_MEMBERS_1 | FileCheck %s -check-prefix=OPTIONAL_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPTIONAL_MEMBERS_2 | FileCheck %s -check-prefix=OPTIONAL_MEMBERS_2

@objc
protocol HasOptionalMembers1 {
  optional func optionalInstanceFunc() -> Int
  optional static func optionalClassFunc() -> Int

  optional var optionalInstanceProperty: Int { get }
  optional static var optionalClassProperty: Int { get }
}

func sanityCheck1(a: HasOptionalMembers1) {
  func isOptionalInt(inout a: Int?) {}

  var result1 = a.optionalInstanceFunc?()
  isOptionalInt(&result1)

  var result2 = a.optionalInstanceProperty
  isOptionalInt(&result2)
}

// NO_ERRORS_UP_TO_HERE

func optionalMembers1(a: HasOptionalMembers1) {
  a.#^OPTIONAL_MEMBERS_1^#
}
// OPTIONAL_MEMBERS_1: Begin completions, 2 items
// OPTIONAL_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   optionalInstanceFunc!()[#Int#]{{$}}
// OPTIONAL_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal:      optionalInstanceProperty[#Int?#]{{$}}
// OPTIONAL_MEMBERS_1: End completions

func optionalMembers2<T : HasOptionalMembers1>(a: T) {
  T.#^OPTIONAL_MEMBERS_2^#
}
// OPTIONAL_MEMBERS_2: Begin completions, 3 items
// OPTIONAL_MEMBERS_2-DAG: Decl[InstanceMethod]/Super:         optionalInstanceFunc!({#self: Self#})[#() -> Int#]{{$}}
// OPTIONAL_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           optionalClassFunc!()[#Int#]{{$}}
// OPTIONAL_MEMBERS_2-DAG: Decl[StaticVar]/Super:              optionalClassProperty[#Int?#]{{$}}
// OPTIONAL_MEMBERS_2: End completions

