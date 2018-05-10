// RUN: %target-swift-ide-test -print-comments -source-filename %s | %FileCheck %s

protocol ParentProtocol1 {
  /// ParentProtocol1.onlyParent1()
  func onlyParent1()
  // CHECK: Func/ParentProtocol1.onlyParent1 {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>onlyParent1()</Name><USR>s:14swift_ide_test15ParentProtocol1P11onlyParent1yyF</USR><Declaration>func onlyParent1()</Declaration><CommentParts><Abstract><Para>ParentProtocol1.onlyParent1()</Para></Abstract></CommentParts></Function>]

  /// ParentProtocol.onlyParent1Var
  var onlyParent1Var: Int { get }
  // CHECK: Var/ParentProtocol1.onlyParent1Var {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>onlyParent1Var</Name><USR>s:14swift_ide_test15ParentProtocol1P14onlyParent1VarSivp</USR><Declaration>var onlyParent1Var: Int { get }</Declaration><CommentParts><Abstract><Para>ParentProtocol.onlyParent1Var</Para></Abstract></CommentParts></Other>]

  /// ParentProtocol.subscript(index:)
  subscript(index: Int) -> Int { get }
  // CHECK: Subscript/ParentProtocol1.subscript {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>subscript(_:)</Name><USR>{{.*}}</USR><Declaration>subscript(index: Int) -&gt; Int { get }</Declaration><CommentParts><Abstract><Para>ParentProtocol.subscript(index:)</Para></Abstract></CommentParts></Other>]

  /// ParentProtocol.AssocType
  associatedtype AssocType
  // CHECK: AssociatedType/ParentProtocol1.AssocType {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>AssocType</Name><USR>s:14swift_ide_test15ParentProtocol1P9AssocTypeQa</USR><Declaration>associatedtype AssocType</Declaration><CommentParts><Abstract><Para>ParentProtocol.AssocType</Para></Abstract></CommentParts></Other>]

  /// ParentProtocol1.commonParentRequirement()
  func commonParentRequirement()
  // CHECKL: Func/ParentProtocol1.commonParentRequirement {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>commonParentRequirement()</Name><USR>s:14swift_ide_test15ParentProtocol1P06commonD11RequirementyyF</USR><Declaration>func commonParentRequirement()</Declaration><CommentParts><Abstract><Para>ParentProtocol1.commonParentRequirement()</Para></Abstract></CommentParts></Function>]

  /// ParentProtocol1.commonRequirementWithDocComment()
  func commonRequirementWithDocComment()
  // CHECK: Func/ParentProtocol1.commonRequirementWithDocComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>commonRequirementWithDocComment()</Name><USR>s:14swift_ide_test15ParentProtocol1P31commonRequirementWithDocCommentyyF</USR><Declaration>func commonRequirementWithDocComment()</Declaration><CommentParts><Abstract><Para>ParentProtocol1.commonRequirementWithDocComment()</Para></Abstract></CommentParts></Function>]

  /// ParentProtocol1.commonRequirementWithoutDocComment()
  func commonRequirementWithoutDocComment()
  // CHECK: Func/ParentProtocol1.commonRequirementWithoutDocComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>commonRequirementWithoutDocComment()</Name><USR>s:14swift_ide_test15ParentProtocol1P34commonRequirementWithoutDocCommentyyF</USR><Declaration>func commonRequirementWithoutDocComment()</Declaration><CommentParts><Abstract><Para>ParentProtocol1.commonRequirementWithoutDocComment()</Para></Abstract></CommentParts></Function>]
}

protocol ParentProtocol2 {
  /// ParentProtocol2.onlyParent2()
  func onlyParent2()
  // CHECK: Func/ParentProtocol2.onlyParent2 {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>onlyParent2()</Name><USR>s:14swift_ide_test15ParentProtocol2P11onlyParent2yyF</USR><Declaration>func onlyParent2()</Declaration><CommentParts><Abstract><Para>ParentProtocol2.onlyParent2()</Para></Abstract></CommentParts></Function>]

  /// ParentProtocol2.commonParentRequirement()
  func commonParentRequirement()
  // CHECK: Func/ParentProtocol2.commonParentRequirement {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>commonParentRequirement()</Name><USR>s:14swift_ide_test15ParentProtocol2P06commonD11RequirementyyF</USR><Declaration>func commonParentRequirement()</Declaration><CommentParts><Abstract><Para>ParentProtocol2.commonParentRequirement()</Para></Abstract></CommentParts></Function>]

  /// ParentProtocol2.commonRequirementWithDocComment()
  func commonRequirementWithDocComment()
  // CHECK: Func/ParentProtocol2.commonRequirementWithDocComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>commonRequirementWithDocComment()</Name><USR>s:14swift_ide_test15ParentProtocol2P31commonRequirementWithDocCommentyyF</USR><Declaration>func commonRequirementWithDocComment()</Declaration><CommentParts><Abstract><Para>ParentProtocol2.commonRequirementWithDocComment()</Para></Abstract></CommentParts></Function>]

  /// ParentProtocol2.commonRequirementWithoutDocComment()
  func commonRequirementWithoutDocComment()
  // CHECK: Func/ParentProtocol2.commonRequirementWithoutDocComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>commonRequirementWithoutDocComment()</Name><USR>s:14swift_ide_test15ParentProtocol2P34commonRequirementWithoutDocCommentyyF</USR><Declaration>func commonRequirementWithoutDocComment()</Declaration><CommentParts><Abstract><Para>ParentProtocol2.commonRequirementWithoutDocComment()</Para></Abstract></CommentParts></Function>]
}

protocol ChildProtocol : ParentProtocol1, ParentProtocol2 {
  /// ChildProtocol.commonRequirementWithDocComment()
  func commonRequirementWithDocComment()
  // CHECK: Func/ChildProtocol.commonRequirementWithDocComment {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>commonRequirementWithDocComment()</Name><USR>s:14swift_ide_test13ChildProtocolP31commonRequirementWithDocCommentyyF</USR><Declaration>func commonRequirementWithDocComment()</Declaration><CommentParts><Abstract><Para>ChildProtocol.commonRequirementWithDocComment()</Para></Abstract></CommentParts></Function>]

  // This should show nothing because there are two inherited requirements.
  func commonRequirementWithoutDocComment()
  // CHECK: Func/ChildProtocol.commonRequirementWithoutDocComment {{.*}} DocCommentAsXML=none
}

// Test that ChildProtocol's default implementation for requirements
// come from the right place.
extension ChildProtocol {
  // Should come from ParentProtocol1.
  func onlyParent1() {}
  // CHECK: Func/onlyParent1 {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>onlyParent1()</Name><USR>s:14swift_ide_test15ParentProtocol1P11onlyParent1yyF</USR><Declaration>func onlyParent1()</Declaration><CommentParts><Abstract><Para>ParentProtocol1.onlyParent1()</Para></Abstract></CommentParts></Function>]

  // Should come from ParentProtocol1.
  var onlyParent1Var: Int { return 0 }
  // CHECK: Var/onlyParent1Var {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>onlyParent1Var</Name><USR>s:14swift_ide_test15ParentProtocol1P14onlyParent1VarSivp</USR><Declaration>var onlyParent1Var: Int { get }</Declaration><CommentParts><Abstract><Para>ParentProtocol.onlyParent1Var</Para></Abstract></CommentParts></Other>]

  // Should come from ParentProtocol1.
  subscript(index: Int) -> Int { return 0 }
  // CHECK: Subscript/subscript {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>subscript(_:)</Name><USR>{{.*}}</USR><Declaration>subscript(index: Int) -&gt; Int { get }</Declaration><CommentParts><Abstract><Para>ParentProtocol.subscript(index:)</Para></Abstract></CommentParts></Other>]

  // Should come from ParentProtocol1.
  typealias AssocType = Int
  // CHECK: TypeAlias/AssocType {{.*}} DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>AssocType</Name><USR>s:14swift_ide_test15ParentProtocol1P9AssocTypeQa</USR><Declaration>associatedtype AssocType</Declaration><CommentParts><Abstract><Para>ParentProtocol.AssocType</Para></Abstract></CommentParts></Other>]

  // Should come from ParentProtocol2.
  func onlyParent2() {}
  // CHECK: Func/onlyParent2 {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>onlyParent2()</Name><USR>s:14swift_ide_test15ParentProtocol2P11onlyParent2yyF</USR><Declaration>func onlyParent2()</Declaration><CommentParts><Abstract><Para>ParentProtocol2.onlyParent2()</Para></Abstract></CommentParts></Function>]

  // Should show nothing because the requirement is in both parents.
  func commonParentRequirement() {}
  // CHECK: Func/commonParentRequirement {{.*}} DocCommentAsXML=none

  // Should show nothing because the requirement is in both parents.
  func commonRequirementWithDocComment() {}
  // CHECK: Func/commonRequirementWithDocComment {{.*}} DocCommentAsXML=none

  // Should show nothing because there are multiple requirements.
  func commonRequirementWithoutDocComment() {}
  // CHECK: Func/commonRequirementWithoutDocComment {{.*}} DocCommentAsXML=none
}

