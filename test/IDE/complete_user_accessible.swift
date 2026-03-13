/// Check that serialized non user accessible functions are not autocompleted.
/// rdar://problem/53891642
/// https://github.com/apple/swift/issues/50003

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/complete_user_accessibility_helper.swift -module-name helper -emit-module-path %t/helper.swiftmodule
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=USER-ACCESS -I %t | %FileCheck %s -check-prefix=USER-ACCESS

import helper

{
  _ = MyEnum.#^USER-ACCESS^#
// USER-ACCESS-DAG:      Keyword[self]/CurrNominal:          self[#MyEnum.Type#]; name=self
// USER-ACCESS-DAG:      Keyword/CurrNominal:                Type[#MyEnum.Type#]; name=Type
// USER-ACCESS-DAG:      Decl[EnumElement]/CurrNominal:      foo[#MyEnum#]; name=foo
// USER-ACCESS-DAG:      Decl[EnumElement]/CurrNominal:      bar[#MyEnum#]; name=bar
// USER-ACCESS-NOT:      __derived_enum_equals
}
