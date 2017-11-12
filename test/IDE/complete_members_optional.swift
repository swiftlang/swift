// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-objc-attr-requires-foundation-module %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPTIONAL_MEMBERS_1 | %FileCheck %s -check-prefix=OPTIONAL_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPTIONAL_MEMBERS_2 | %FileCheck %s -check-prefix=OPTIONAL_MEMBERS_2

@objc
protocol HasOptionalMembers1 {
  @objc optional func optionalInstanceFunc() -> Int
  @objc optional static func optionalClassFunc() -> Int

  @objc optional var optionalInstanceProperty: Int { get }
  @objc optional static var optionalClassProperty: Int { get }
}

func sanityCheck1(_ a: HasOptionalMembers1) {
  func isOptionalInt(_ a: inout Int?) {}

  var result1 = a.optionalInstanceFunc?()
  isOptionalInt(&result1)

  var result2 = a.optionalInstanceProperty
  isOptionalInt(&result2)
}

// NO_ERRORS_UP_TO_HERE

func optionalMembers1(_ a: HasOptionalMembers1) {
  a.#^OPTIONAL_MEMBERS_1^#
}
// OPTIONAL_MEMBERS_1: Begin completions, 2 items
// OPTIONAL_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   optionalInstanceFunc!()[#Int#]{{; name=.+$}}
// OPTIONAL_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal:      optionalInstanceProperty[#Int?#]{{; name=.+$}}
// OPTIONAL_MEMBERS_1: End completions

func optionalMembers2<T : HasOptionalMembers1>(_ a: T) {
  T.#^OPTIONAL_MEMBERS_2^#
}
// OPTIONAL_MEMBERS_2: Begin completions, 3 items
// OPTIONAL_MEMBERS_2-DAG: Decl[InstanceMethod]/Super:         optionalInstanceFunc!({#self: HasOptionalMembers1#})[#() -> Int#]{{; name=.+$}}
// OPTIONAL_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           optionalClassFunc!()[#Int#]{{; name=.+$}}
// OPTIONAL_MEMBERS_2-DAG: Decl[StaticVar]/Super:              optionalClassProperty[#Int?#]{{; name=.+$}}
// OPTIONAL_MEMBERS_2: End completions
