// RUN: %target-swift-ide-test -print-comments -source-filename %s | %FileCheck %s
// REQUIRES: no_asan

class Base {
  func noComments() {}
  // CHECK: Func/Base.noComments {{.*}} DocCommentAsXML=none

  /// Base
  func funcNoDerivedComment() {}
  // CHECK: Func/Base.funcNoDerivedComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>funcNoDerivedComment()</Name><USR>s:FC14swift_ide_test4Base20funcNoDerivedCommentFT_T_</USR><Declaration>func funcNoDerivedComment()</Declaration><Abstract><Para>Base</Para></Abstract></Function>]

  /// Base
  func funcWithDerivedComment() {}
  // CHECK: Func/Base.funcWithDerivedComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>funcWithDerivedComment()</Name><USR>s:FC14swift_ide_test4Base22funcWithDerivedCommentFT_T_</USR><Declaration>func funcWithDerivedComment()</Declaration><Abstract><Para>Base</Para></Abstract></Function>]

  /// Base
  var varNoDerivedComment: Bool {
    return false
  }
  // CHECK: Var/Base.varNoDerivedComment {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>varNoDerivedComment</Name><USR>s:vC14swift_ide_test4Base19varNoDerivedCommentSb</USR><Declaration>var varNoDerivedComment: Bool { get }</Declaration><Abstract><Para>Base</Para></Abstract></Other>]

  /// Base
  var varWithDerivedComment: Bool {
    return false
  }
  // CHECK: Var/Base.varWithDerivedComment {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>varWithDerivedComment</Name><USR>s:vC14swift_ide_test4Base21varWithDerivedCommentSb</USR><Declaration>var varWithDerivedComment: Bool { get }</Declaration><Abstract><Para>Base</Para></Abstract></Other>]
}

class Derived : Base {
  override func noComments() {}
  // CHECK: Func/Derived.noComments {{.*}} DocCommentAsXML=none

  override func funcNoDerivedComment() {}
  // CHECK: Func/Derived.funcNoDerivedComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>funcNoDerivedComment()</Name><USR>s:FC14swift_ide_test7Derived20funcNoDerivedCommentFT_T_</USR><Declaration>override func funcNoDerivedComment()</Declaration><Abstract><Para>Base</Para></Abstract><Discussion><Note><Para>This documentation comment was inherited from <codeVoice>Base</codeVoice>.</Para></Note></Discussion></Function>]

  /// Derived
  override func funcWithDerivedComment() {}
  // CHECK: Func/Derived.funcWithDerivedComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>funcWithDerivedComment()</Name><USR>s:FC14swift_ide_test7Derived22funcWithDerivedCommentFT_T_</USR><Declaration>override func funcWithDerivedComment()</Declaration><Abstract><Para>Derived</Para></Abstract></Function>]

  override var varNoDerivedComment: Bool {
    return false
  }
  // CHECK: Var/Derived.varNoDerivedComment {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>varNoDerivedComment</Name><USR>s:vC14swift_ide_test7Derived19varNoDerivedCommentSb</USR><Declaration>override var varNoDerivedComment: Bool { get }</Declaration><Abstract><Para>Base</Para></Abstract><Discussion><Note><Para>This documentation comment was inherited from <codeVoice>Base</codeVoice>.</Para></Note></Discussion></Other>]

  // Derived
  override var varWithDerivedComment : Bool {
    return true
  }
  // CHECK: Var/Derived.varWithDerivedComment {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>varWithDerivedComment</Name><USR>s:vC14swift_ide_test7Derived21varWithDerivedCommentSb</USR><Declaration>override var varWithDerivedComment: Bool { get }</Declaration><Abstract><Para>Base</Para></Abstract><Discussion><Note><Para>This documentation comment was inherited from <codeVoice>Base</codeVoice>.</Para></Note></Discussion></Other>]
}

