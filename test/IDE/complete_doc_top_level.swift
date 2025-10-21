/// top-level var 1 comment
var topLevelVar1 = "Swift"

/// top-level var 2 comment
///
/// the meaning of life
var topLevelVar2 = 42

/// top-level func 1 comment
///
/// Does stuff.
func topLevelFunc1() {}

/// top-level func 2 comment
func topLevelFunc2() {}

/// struct S1
///
/// More info on struct S
struct S1 {}

/// C1 description
///
/// does some work
class C1 {}

/// P1 description
///
/// defines some functionality
protocol P1 {}

struct S2: P1 {}

#^TOP_LEVEL^#

// RUN: %target-swift-ide-test -code-completion -source-filename %s -module-name CompleteDocTest -code-completion-comments -code-completion-sort-by-name -code-completion-token=TOP_LEVEL | %FileCheck %s --check-prefix=TOP-LEVEL

// TOP-LEVEL-LABEL: Decl[Class]/CurrModule:             C1[#C1#]; name=C1;
// TOP-LEVEL-SAME:  briefcomment=C1 description;
// TOP-LEVEL-SAME:  xmlcomment=<Class file="{{.*}}" line="25" column="7"><Name>C1</Name><USR>s:15CompleteDocTest2C1C</USR><Declaration>class C1</Declaration><CommentParts><Abstract><Para>C1 description</Para></Abstract><Discussion><Para>does some work</Para></Discussion></CommentParts></Class>;
// TOP-LEVEL-SAME:  rawcomment=C1 description
// TOP-LEVEL-EMPTY:
// TOP-LEVEL-NEXT:  does some work

// TOP-LEVEL-LABEL: Decl[Protocol]/CurrModule/Flair[RareType]: P1[#P1#]; name=P1;
// TOP-LEVEL-SAME:  briefcomment=P1 description;
// TOP-LEVEL-SAME:  xmlcomment=<Class file="{{.*}}" line="30" column="10"><Name>P1</Name><USR>s:15CompleteDocTest2P1P</USR><Declaration>protocol P1</Declaration><CommentParts><Abstract><Para>P1 description</Para></Abstract><Discussion><Para>defines some functionality</Para></Discussion></CommentParts></Class>;
// TOP-LEVEL-SAME:  rawcomment=P1 description
// TOP-LEVEL-EMPTY:
// TOP-LEVEL-NEXT:  defines some functionality

// TOP-LEVEL-LABEL: Decl[Struct]/CurrModule:            S1[#S1#]; name=S1;
// TOP-LEVEL-SAME:  briefcomment=struct S1;
// TOP-LEVEL-SAME:  xmlcomment=<Class file="{{.*}}" line="20" column="8"><Name>S1</Name><USR>s:15CompleteDocTest2S1V</USR><Declaration>struct S1</Declaration><CommentParts><Abstract><Para>struct S1</Para></Abstract><Discussion><Para>More info on struct S</Para></Discussion></CommentParts></Class>;
// TOP-LEVEL-SAME:  rawcomment=struct S1
// TOP-LEVEL-EMPTY:
// TOP-LEVEL-NEXT:  More info on struct S

// TOP-LEVEL-LABEL: Decl[Struct]/CurrModule:            S2[#S2#]; name=S2{{$}}

// TOP-LEVEL-LABEL: Decl[FreeFunction]/CurrModule:      topLevelFunc1()[#Void#]; name=topLevelFunc1();
// TOP-LEVEL-SAME:  briefcomment=top-level func 1 comment;
// TOP-LEVEL-SAME:  xmlcomment=<Function file="{{.*}}" line="12" column="6"><Name>topLevelFunc1()</Name><USR>s:15CompleteDocTest13topLevelFunc1yyF</USR><Declaration>func topLevelFunc1()</Declaration><CommentParts><Abstract><Para>top-level func 1 comment</Para></Abstract><Discussion><Para>Does stuff.</Para></Discussion></CommentParts></Function>;
// TOP-LEVEL-SAME:  rawcomment=top-level func 1 comment
// TOP-LEVEL-EMPTY:
// TOP-LEVEL-NEXT:  Does stuff.

// TOP-LEVEL-LABEL: Decl[FreeFunction]/CurrModule:      topLevelFunc2()[#Void#]; name=topLevelFunc2();
// TOP-LEVEL-SAME:  briefcomment=top-level func 2 comment;
// TOP-LEVEL-SAME:  xmlcomment=<Function file="{{.*}}" line="15" column="6"><Name>topLevelFunc2()</Name><USR>s:15CompleteDocTest13topLevelFunc2yyF</USR><Declaration>func topLevelFunc2()</Declaration><CommentParts><Abstract><Para>top-level func 2 comment</Para></Abstract></CommentParts></Function>;
// TOP-LEVEL-SAME:  rawcomment=top-level func 2 comment

// TOP-LEVEL-LABEL: Decl[GlobalVar]/Local:              topLevelVar1[#String#]; name=topLevelVar1;
// TOP-LEVEL-SAME:  briefcomment=top-level var 1 comment;
// TOP-LEVEL-SAME:  xmlcomment=<Other file="{{.*}}" line="2" column="5"><Name>topLevelVar1</Name><USR>s:15CompleteDocTest12topLevelVar1SSvp</USR><Declaration>var topLevelVar1: String</Declaration><CommentParts><Abstract><Para>top-level var 1 comment</Para></Abstract></CommentParts></Other>;
// TOP-LEVEL-SAME:  rawcomment=top-level var 1 comment

// TOP-LEVEL-LABEL: Decl[GlobalVar]/Local:              topLevelVar2[#Int#]; name=topLevelVar2;
// TOP-LEVEL-SAME:  briefcomment=top-level var 2 comment;
// TOP-LEVEL-SAME:  xmlcomment=<Other file="{{.*}}" line="7" column="5"><Name>topLevelVar2</Name><USR>s:15CompleteDocTest12topLevelVar2Sivp</USR><Declaration>var topLevelVar2: Int</Declaration><CommentParts><Abstract><Para>top-level var 2 comment</Para></Abstract><Discussion><Para>the meaning of life</Para></Discussion></CommentParts></Other>;
// TOP-LEVEL-SAME:  rawcomment=top-level var 2 comment
// TOP-LEVEL-EMPTY:
// TOP-LEVEL-NEXT:  the meaning of life
