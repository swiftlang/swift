// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_ATTR_1 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_DECL_ATTR_1 | FileCheck %s -check-prefix=ERROR_COMMON

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

@#^TOP_LEVEL_ATTR_1^# class TopLevelDeclAttr1 {}

class MemberDeclAttribute {
  @#^MEMBER_DECL_ATTR_1^# func memberDeclAttr1() {}
}
