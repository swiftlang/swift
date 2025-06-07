// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_ATTR_1 -code-completion-keywords=false | %FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_DECL_ATTR_1 -code-completion-keywords=false | %FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ATTRARG_MEMBER | %FileCheck %s -check-prefix=MEMBER_MyValue
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ATTRARG_MEMBER_IN_CLOSURE | %FileCheck %s -check-prefix=MEMBER_MyValue

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Keyword/

@#^TOP_LEVEL_ATTR_1^# class TopLevelDeclAttr1 {}

class MemberDeclAttribute {
  @#^MEMBER_DECL_ATTR_1^# func memberDeclAttr1() {}
}

struct MyValue {
  init() {}
  static var val: Int
}

// MEMBER_MyValue: Begin completions, 4 items
// MEMBER_MyValue-DAG: Keyword[self]/CurrNominal:          self[#MyValue.Type#];
// MEMBER_MyValue-DAG: Keyword/CurrNominal:                Type[#MyValue.Type#];
// MEMBER_MyValue-DAG: Decl[Constructor]/CurrNominal:      init()[#MyValue#];
// MEMBER_MyValue-DAG: Decl[StaticVar]/CurrNominal:        val[#Int#];

class TestUnknownDanglingAttr1 {
  @UnknownAttr(arg: MyValue.#^ATTRARG_MEMBER^#)
}
class TestUnknownDanglingAttr2 {
  @UnknownAttr(arg: { MyValue.#^ATTRARG_MEMBER_IN_CLOSURE^# })
}
