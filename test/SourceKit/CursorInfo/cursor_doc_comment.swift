/// Test
/// - Returns: An integer
func test() -> Int {}
// RUN: %sourcekitd-test -req=cursor -pos=%(line - 1):6 %s -- %s | %FileCheck %s

// CHECK-LABEL: DOC COMMENT
// CHECK: Test
// CHECK: - Returns: An integer

// CHECK-LABEL: DOC COMMENT XML
// CHECK: <Function file="{{.*}}" line="3" column="6"><Name>test()</Name><USR>s:18cursor_doc_comment4testSiyF</USR><Declaration>func test() -&gt; Int</Declaration><CommentParts><Abstract><Para>Test</Para></Abstract><ResultDiscussion><Para>An integer</Para></ResultDiscussion></CommentParts></Function>
