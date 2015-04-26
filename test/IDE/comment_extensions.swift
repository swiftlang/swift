// RUN: %target-swift-ide-test -print-comments -source-filename %s -comments-xml-schema %S/../../bindings/xml/comment-xml-schema.rng | FileCheck %s

/// - attention: This function is so hip and exciting, it can't be trusted.
func attention() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>attention()</Name><USR>s:F14swift_ide_test9attentionFT_T_</USR><Declaration>func attention()</Declaration><Discussion><Attention><Para>This function is so hip and exciting, it can&apos;t be trusted.</Para></Attention></Discussion></Function>] CommentXMLValid

/// - author: Stephen
func author() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>author()</Name><USR>s:F14swift_ide_test6authorFT_T_</USR><Declaration>func author()</Declaration><Discussion><Author><Para>Stephen</Para></Author></Discussion></Function>] CommentXMLValid

/// - authors:
///   - Homer
///   - Mark
///   - J.
func authors() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>authors()</Name><USR>s:F14swift_ide_test7authorsFT_T_</USR><Declaration>func authors()</Declaration><Discussion><Authors><Para></Para><List-Bullet><Item><Para>Homer</Para></Item><Item><Para>Mark</Para></Item><Item><Para>J.</Para></Item></List-Bullet></Authors></Discussion></Function>] CommentXMLValid

/// - bug: rdar://problem/8675309
func bug() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>bug()</Name><USR>s:F14swift_ide_test3bugFT_T_</USR><Declaration>func bug()</Declaration><Discussion><Bug><Para>rdar://problem/8675309</Para></Bug></Discussion></Function>] CommentXMLValid

/// - complexity: O(n log2(n))
func complexity() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>complexity()</Name><USR>s:F14swift_ide_test10complexityFT_T_</USR><Declaration>func complexity()</Declaration><Discussion><Complexity><Para>O(n log2(n))</Para></Complexity></Discussion></Function>] CommentXMLValid

/// - copyright: 2015 Apple, Inc.
func copyright() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>copyright()</Name><USR>s:F14swift_ide_test9copyrightFT_T_</USR><Declaration>func copyright()</Declaration><Discussion><Copyright><Para>2015 Apple, Inc.</Para></Copyright></Discussion></Function>] CommentXMLValid

/// - date: Thu Apr 23 22:38:09 PDT 2015
func date() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>date()</Name><USR>s:F14swift_ide_test4dateFT_T_</USR><Declaration>func date()</Declaration><Discussion><Date><Para>Thu Apr 23 22:38:09 PDT 2015</Para></Date></Discussion></Function>] CommentXMLValid

/// - experiment: Try some more. The strawberries taste like strawberries.
func experiment() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>experiment()</Name><USR>s:F14swift_ide_test10experimentFT_T_</USR><Declaration>func experiment()</Declaration><Discussion><Experiment><Para>Try some more. The strawberries taste like strawberries.</Para></Experiment></Discussion></Function>] CommentXMLValid

/// - invariant: x not nil
struct Invariant {
  let x: Int!
}
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>Invariant</Name><USR>s:V14swift_ide_test9Invariant</USR><Declaration>struct Invariant</Declaration><Discussion><Invariant><Para>x not nil</Para></Invariant></Discussion></Class>] CommentXMLValid
// CHECK: {{.*}}DocCommentAsXML=none

/// - note: This function is very hip and exciting.
func note() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>note()</Name><USR>s:F14swift_ide_test4noteFT_T_</USR><Declaration>func note()</Declaration><Discussion><Note><Para>This function is very hip and exciting.</Para></Note></Discussion></Function>] CommentXMLValid

/// - postcondition: x is unchanged
func postcondition(inout x: Int) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>postcondition(_:)</Name><USR>s:F14swift_ide_test13postconditionFRSiT_</USR><Declaration>func postcondition(inout x: Int)</Declaration><Discussion><Postcondition><Para>x is unchanged</Para></Postcondition></Discussion></Function>] CommentXMLValid
// CHECK: {{.*}}DocCommentAsXML=none

/// - precondition: `x < 100`
func precondition(x: Int) {
  assert(x < 100)
}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>precondition(_:)</Name><USR>s:F14swift_ide_test12preconditionFSiT_</USR><Declaration>func precondition(x: Int)</Declaration><Discussion><Precondition><Para><codeVoice>x &lt; 100</codeVoice></Para></Precondition></Discussion></Function>] CommentXMLValid
// CHECK: {{.*}}DocCommentAsXML=none

/// - remark: Always, no, never forget to check your references.
func remark() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>remark()</Name><USR>s:F14swift_ide_test6remarkFT_T_</USR><Declaration>func remark()</Declaration><Discussion><Remark><Para>Always, no, never forget to check your references.</Para></Remark></Discussion></Function>] CommentXMLValid

/// - remarks:
///   - Never let a bear approach you.
func remarks() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>remarks()</Name><USR>s:F14swift_ide_test7remarksFT_T_</USR><Declaration>func remarks()</Declaration><Discussion><Remarks><Para></Para><List-Bullet><Item><Para>Never let a bear approach you.</Para></Item></List-Bullet></Remarks></Discussion></Function>] CommentXMLValid

/// - seealso: the pie (it's very good).
func see() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>see()</Name><USR>s:F14swift_ide_test3seeFT_T_</USR><Declaration>func see()</Declaration><Discussion><See><Para>the pie (it&apos;s very good).</Para></See></Discussion></Function>] CommentXMLValid

/// - since: 1809
func since() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>since()</Name><USR>s:F14swift_ide_test5sinceFT_T_</USR><Declaration>func since()</Declaration><Discussion><Since><Para>1809</Para></Since></Discussion></Function>] CommentXMLValid

/// - todo: be
/// - todo: or not to be
func todo() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>todo()</Name><USR>s:F14swift_ide_test4todoFT_T_</USR><Declaration>func todo()</Declaration><Discussion><TODO><Para>be</Para></TODO><TODO><Para>or not to be</Para></TODO></Discussion></Function>] CommentXMLValid

/// - version: Beta.
func version() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>version()</Name><USR>s:F14swift_ide_test7versionFT_T_</USR><Declaration>func version()</Declaration><Discussion><Version><Para>Beta.</Para></Version></Discussion></Function>] CommentXMLValid

/// - warning: Share the road.
func warning() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>warning()</Name><USR>s:F14swift_ide_test7warningFT_T_</USR><Declaration>func warning()</Declaration><Discussion><Warning><Para>Share the road.</Para></Warning></Discussion></Function>] CommentXMLValid
