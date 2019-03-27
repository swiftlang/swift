// RUN: %target-swift-ide-test -print-comments -source-filename %S/Inputs/comment_extensions.swift -comments-xml-schema %S/../../bindings/xml/comment-xml-schema.rng | %FileCheck %s
// REQUIRES: libxml2

// Content is in separate file in ./Inputs due to the "requires" keyword getting
// recognized by lit.

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}} line="{{.*}}" column="{{.*}}"><Name>attention()</Name><USR>s:14swift_ide_test9attentionyyF</USR><Declaration>func attention()</Declaration><CommentParts><Discussion><Attention><Para>This function is so hip and exciting, it can’t be trusted.</Para></Attention></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>author()</Name><USR>s:14swift_ide_test6authoryyF</USR><Declaration>func author()</Declaration><CommentParts><Discussion><Author><Para>Stephen</Para></Author></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>authors()</Name><USR>s:14swift_ide_test7authorsyyF</USR><Declaration>func authors()</Declaration><CommentParts><Discussion><Authors><Para></Para><List-Bullet><Item><Para>Homer</Para></Item><Item><Para>Mark</Para></Item><Item><Para>J.</Para></Item></List-Bullet></Authors></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>bug()</Name><USR>s:14swift_ide_test3bugyyF</USR><Declaration>func bug()</Declaration><CommentParts><Discussion><Bug><Para>rdar://problem/8675309</Para></Bug></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>complexity()</Name><USR>s:14swift_ide_test10complexityyyF</USR><Declaration>func complexity()</Declaration><CommentParts><Discussion><Complexity><Para>O(n log2(n))</Para></Complexity></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>copyright()</Name><USR>s:14swift_ide_test9copyrightyyF</USR><Declaration>func copyright()</Declaration><CommentParts><Discussion><Copyright><Para>2015 Apple, Inc.</Para></Copyright></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>date()</Name><USR>s:14swift_ide_test4dateyyF</USR><Declaration>func date()</Declaration><CommentParts><Discussion><Date><Para>Thu Apr 23 22:38:09 PDT 2015</Para></Date></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>experiment()</Name><USR>s:14swift_ide_test10experimentyyF</USR><Declaration>func experiment()</Declaration><CommentParts><Discussion><Experiment><Para>Try some more. The strawberries taste like strawberries.</Para></Experiment></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>Invariant</Name><USR>s:14swift_ide_test9InvariantV</USR><Declaration>struct Invariant</Declaration><CommentParts><Discussion><Invariant><Para>x not nil</Para></Invariant></Discussion></CommentParts></Class>]

// CHECK: {{.*}}DocCommentAsXML=none

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>note()</Name><USR>s:14swift_ide_test4noteyyF</USR><Declaration>func note()</Declaration><CommentParts><Discussion><Note><Para>This function is very hip and exciting.</Para></Note></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>postcondition(_:)</Name><USR>s:14swift_ide_test13postconditionyySizF</USR><Declaration>func postcondition(_ x: inout Int)</Declaration><CommentParts><Discussion><Postcondition><Para>x is unchanged</Para></Postcondition></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=none

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>precondition(_:)</Name><USR>s:14swift_ide_test12preconditionyySiF</USR><Declaration>func precondition(_ x: Int)</Declaration><CommentParts><Discussion><Precondition><Para><codeVoice>x &lt; 100</codeVoice></Para></Precondition></Discussion></CommentParts></Function>]
// CHECK: {{.*}}DocCommentAsXML=none

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>remark()</Name><USR>s:14swift_ide_test6remarkyyF</USR><Declaration>func remark()</Declaration><CommentParts><Discussion><Remark><Para>Always, no, never forget to check your references.</Para></Remark></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>remarks()</Name><USR>s:14swift_ide_test7remarksyyF</USR><Declaration>func remarks()</Declaration><CommentParts><Discussion><Remarks><Para></Para><List-Bullet><Item><Para>Never let a bear approach you.</Para></Item></List-Bullet></Remarks></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>requires()</Name><USR>s:14swift_ide_test8requiresyyF</USR><Declaration>func requires()</Declaration><CommentParts><Discussion><Requires><Para></Para><List-Bullet><Item><Para>explicit package name. Just kidding!</Para></Item></List-Bullet></Requires></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>see()</Name><USR>s:14swift_ide_test3seeyyF</USR><Declaration>func see()</Declaration><CommentParts><Discussion><See><Para>the pie (it’s very good).</Para></See></Discussion></CommentParts></Function>] CommentXMLValid

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>since()</Name><USR>s:14swift_ide_test5sinceyyF</USR><Declaration>func since()</Declaration><CommentParts><Discussion><Since><Para>1809</Para></Since></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>todo()</Name><USR>s:14swift_ide_test4todoyyF</USR><Declaration>func todo()</Declaration><CommentParts><Discussion><TODO><Para>be</Para></TODO><TODO><Para>or not to be</Para></TODO></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>version()</Name><USR>s:14swift_ide_test7versionyyF</USR><Declaration>func version()</Declaration><CommentParts><Discussion><Version><Para>Beta.</Para></Version></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>warning()</Name><USR>s:14swift_ide_test7warningyyF</USR><Declaration>func warning()</Declaration><CommentParts><Discussion><Warning><Para>Share the road.</Para></Warning></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>imageWithEmptyURLAndBogusTitle()</Name><USR>s:14swift_ide_test30imageWithEmptyURLAndBogusTitleyyF</USR><Declaration>func imageWithEmptyURLAndBogusTitle()</Declaration><CommentParts><Abstract><Para><rawHTML><![CDATA[<img src="" alt="/bogus/url/as/title"/>]]></rawHTML></Para></Abstract></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>imageTitleAndAlt()</Name><USR>s:14swift_ide_test16imageTitleAndAltyyF</USR><Declaration>func imageTitleAndAlt()</Declaration><CommentParts><Abstract><Para>Brief.</Para></Abstract><Discussion><Para><rawHTML><![CDATA[<img src="/swift.png" title="Image Title" alt="Image Alt"/>]]></rawHTML></Para></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>imageAlt()</Name><USR>s:14swift_ide_test8imageAltyyF</USR><Declaration>func imageAlt()</Declaration><CommentParts><Abstract><Para>Brief.</Para></Abstract><Discussion><Para><rawHTML><![CDATA[<img src="/swift.png" alt="Image Alt"/>]]></rawHTML></Para></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>imageTitle()</Name><USR>s:14swift_ide_test10imageTitleyyF</USR><Declaration>func imageTitle()</Declaration><CommentParts><Abstract><Para>Brief.</Para></Abstract><Discussion><Para><rawHTML><![CDATA[<img src="/swift.png" title="Image Title" alt="Image Alt"/>]]></rawHTML></Para></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>urlWithQueryString()</Name><USR>s:14swift_ide_test18urlWithQueryStringyyF</USR><Declaration>func urlWithQueryString()</Declaration><CommentParts><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>Test <Link href="http://apple.com?a=1&amp;b=1&amp;c=abc">a link</Link></Para></Discussion></CommentParts></Function>]

// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>imageWithAmpersandsInTitleAndAlt()</Name><USR>s:14swift_ide_test32imageWithAmpersandsInTitleAndAltyyF</USR><Declaration>func imageWithAmpersandsInTitleAndAlt()</Declaration><CommentParts><Abstract><Para>Brief.</Para></Abstract><Discussion><Para><rawHTML><![CDATA[<img src="http://apple.com" title="&&&" alt="&&&"/>]]></rawHTML></Para></Discussion></CommentParts></Function>]
