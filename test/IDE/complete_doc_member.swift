// RUN: %batch-code-completion -source-filename %s -module-name CompleteDocTest -code-completion-comments -code-completion-sort-by-name

protocol P1 {
  /// requirement1 doc comment
  ///
  /// more description
  var requirement1: String { get }

  func requirement2()
  
  /// requirement3 comment in P1
  func requirement3(x: Int)
}

/// struct S
///
/// More info on struct S
struct S1: P1 {
  /// instanceVar1 description
  var instanceVar1: String = "OK"
  
  /// instanceVar2 description
  ///
  /// Euler-Mascheroni constant
  var instanceVar2: Double = 0.5772156649
  
  /// struct instance func comment
  ///
  /// Does stuff with the instance.
  func instanceFunc1() {}
  
  /// struct instance func comment
  ///
  /// Does stuff with the instance.
  ///
  /// - Parameters:
  ///   - x: what is x?
  func instanceFunc2(x: String) {}
  
  /// subscript on struct S
  ///
  /// - Parameters:
  ///   - index: an index into S1
  subscript(index: Int) {}
  
  var requirement1: String = "name"
  
  func requirement2() {}
  
  /// requirement3 comment in S1
  func requirement3() {}
}

extension S1 {
  /// doc for extendedFunc1
  func extendedFunc1(x: Int, y: Int) -> Int {
    return x + y * 2
  }
  
  func extendedFunc2() {}
}

func testWithDot() {
  S1().#^MEMBER_DOT^#
}

// MEMBER_DOT:      Begin completions, 11 items

// MEMBER_DOT-NEXT:      Decl[InstanceMethod]/CurrNominal:   extendedFunc1({#x: Int#}, {#y: Int#})[#Int#]; name=extendedFunc1(x:y:);
// MEMBER_DOT-SAME: briefcomment=doc for extendedFunc1;
// MEMBER_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="56" column="8"><Name>extendedFunc1(x:y:)</Name><USR>s:15CompleteDocTest2S1V13extendedFunc11x1yS2i_SitF</USR><Declaration>func extendedFunc1(x: Int, y: Int) -&gt; Int</Declaration><CommentParts><Abstract><Para>doc for extendedFunc1</Para></Abstract></CommentParts></Function>;
// MEMBER_DOT-SAME: rawcomment=doc for extendedFunc1

// MEMBER_DOT-NEXT:      Decl[InstanceMethod]/CurrNominal:   extendedFunc2()[#Void#]; name=extendedFunc2(){{$}}

// MEMBER_DOT-NEXT:      Decl[InstanceMethod]/CurrNominal:   instanceFunc1()[#Void#]; name=instanceFunc1();
// MEMBER_DOT-SAME: briefcomment=struct instance func comment;
// MEMBER_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="30" column="8"><Name>instanceFunc1()</Name><USR>s:15CompleteDocTest2S1V13instanceFunc1yyF</USR><Declaration>func instanceFunc1()</Declaration><CommentParts><Abstract><Para>struct instance func comment</Para></Abstract><Discussion><Para>Does stuff with the instance.</Para></Discussion></CommentParts></Function>;
// MEMBER_DOT-SAME: rawcomment=struct instance func comment
// MEMBER_DOT-EMPTY:
// MEMBER_DOT-NEXT: Does stuff with the instance.

// MEMBER_DOT-NEXT:      Decl[InstanceMethod]/CurrNominal:   instanceFunc2({#x: String#})[#Void#]; name=instanceFunc2(x:);
// MEMBER_DOT-SAME: briefcomment=struct instance func comment;
// MEMBER_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="38" column="8"><Name>instanceFunc2(x:)</Name><USR>s:15CompleteDocTest2S1V13instanceFunc21xySS_tF</USR><Declaration>func instanceFunc2(x: String)</Declaration><CommentParts><Abstract><Para>struct instance func comment</Para></Abstract><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>what is x?</Para></Discussion></Parameter></Parameters><Discussion><Para>Does stuff with the instance.</Para></Discussion></CommentParts></Function>;
// MEMBER_DOT-SAME: rawcomment=struct instance func comment
// MEMBER_DOT-EMPTY:
// MEMBER_DOT-NEXT: Does stuff with the instance.
// MEMBER_DOT-EMPTY:
// MEMBER_DOT-NEXT: - Parameters:
// MEMBER_DOT-NEXT:   - x: what is x?

// MEMBER_DOT-NEXT:      Decl[InstanceVar]/CurrNominal:      instanceVar1[#String#]; name=instanceVar1;
// MEMBER_DOT-SAME: briefcomment=instanceVar1 description;
// MEMBER_DOT-SAME: xmlcomment=<Other file="{{.*}}" line="20" column="7"><Name>instanceVar1</Name><USR>s:15CompleteDocTest2S1V12instanceVar1SSvp</USR><Declaration>var instanceVar1: String</Declaration><CommentParts><Abstract><Para>instanceVar1 description</Para></Abstract></CommentParts></Other>;
// MEMBER_DOT-SAME: rawcomment=instanceVar1 description

// MEMBER_DOT-NEXT:      Decl[InstanceVar]/CurrNominal:      instanceVar2[#Double#]; name=instanceVar2;
// MEMBER_DOT-SAME: briefcomment=instanceVar2 description;
// MEMBER_DOT-SAME: xmlcomment=<Other file="{{.*}}" line="25" column="7"><Name>instanceVar2</Name><USR>s:15CompleteDocTest2S1V12instanceVar2Sdvp</USR><Declaration>var instanceVar2: Double</Declaration><CommentParts><Abstract><Para>instanceVar2 description</Para></Abstract><Discussion><Para>Euler-Mascheroni constant</Para></Discussion></CommentParts></Other>;
// MEMBER_DOT-SAME: rawcomment=instanceVar2 description
// MEMBER_DOT-EMPTY:
// MEMBER_DOT-NEXT: Euler-Mascheroni constant

// MEMBER_DOT-NEXT:      Decl[InstanceVar]/CurrNominal:      requirement1[#String#]; name=requirement1;
// MEMBER_DOT-SAME: briefcomment=requirement1 doc comment;
// MEMBER_DOT-SAME: xmlcomment=<Other file="{{.*}}" line="7" column="7"><Name>requirement1</Name><USR>s:15CompleteDocTest2P1P12requirement1SSvp</USR><Declaration>var requirement1: String { get }</Declaration><CommentParts><Abstract><Para>requirement1 doc comment</Para></Abstract><Discussion><Para>more description</Para><Note><Para>This documentation comment was inherited from <codeVoice>P1</codeVoice>.</Para></Note></Discussion></CommentParts></Other>;
// MEMBER_DOT-SAME: rawcomment=requirement1 doc comment
// MEMBER_DOT-EMPTY:
// MEMBER_DOT-NEXT: more description

// MEMBER_DOT-NEXT:      Decl[InstanceMethod]/CurrNominal:   requirement2()[#Void#]; name=requirement2(){{$}}

// MEMBER_DOT-NEXT:      Decl[InstanceMethod]/CurrNominal:   requirement3()[#Void#]; name=requirement3();
// MEMBER_DOT-SAME: briefcomment=requirement3 comment in S1;
// MEMBER_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="51" column="8"><Name>requirement3()</Name><USR>s:15CompleteDocTest2S1V12requirement3yyF</USR><Declaration>func requirement3()</Declaration><CommentParts><Abstract><Para>requirement3 comment in S1</Para></Abstract></CommentParts></Function>;
// MEMBER_DOT-SAME: rawcomment=requirement3 comment in S1

// MEMBER_DOT-NEXT:      Decl[InstanceMethod]/Super:         requirement3({#x: Int#})[#Void#]; name=requirement3(x:);
// MEMBER_DOT-SAME: briefcomment=requirement3 comment in P1;
// MEMBER_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="12" column="8"><Name>requirement3(x:)</Name><USR>s:15CompleteDocTest2P1P12requirement31xySi_tF</USR><Declaration>func requirement3(x: Int)</Declaration><CommentParts><Abstract><Para>requirement3 comment in P1</Para></Abstract></CommentParts></Function>;
// MEMBER_DOT-SAME: rawcomment=requirement3 comment in P1

// MEMBER_DOT-NEXT: Keyword[self]/CurrNominal:          self[#S1#]; name=self{{$}}

func testWithNoDot() {
  S1()#^MEMBER_NO_DOT^#
}

// MEMBER_NO_DOT:      Begin completions, 12 items

// MEMBER_NO_DOT-NEXT: Decl[Subscript]/CurrNominal:        [{#(index): Int#}][#<<error type>>#]; name=[:];
// MEMBER_NO_DOT-SAME: briefcomment=subscript on struct S;
// MEMBER_NO_DOT-SAME: xmlcomment=<Other file="{{.*}}" line="44" column="3"><Name>subscript(_:)</Name><USR>s:15CompleteDocTest2S1VyXeXecip</USR><Declaration>subscript(index: Int) -&gt; &lt;&lt;error type&gt;&gt; { get }</Declaration><CommentParts><Abstract><Para>subscript on struct S</Para></Abstract><Parameters><Parameter><Name>index</Name><Direction isExplicit="0">in</Direction><Discussion><Para>an index into S1</Para></Discussion></Parameter></Parameters></CommentParts></Other>;
// MEMBER_NO_DOT-SAME: rawcomment=subscript on struct S
// MEMBER_NO_DOT-EMPTY:
// MEMBER_NO_DOT-NEXT: - Parameters:
// MEMBER_NO_DOT-NEXT:   - index: an index into S1

// MEMBER_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal:   .extendedFunc1({#x: Int#}, {#y: Int#})[#Int#]; name=extendedFunc1(x:y:);
// MEMBER_NO_DOT-SAME: briefcomment=doc for extendedFunc1;
// MEMBER_NO_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="56" column="8"><Name>extendedFunc1(x:y:)</Name><USR>s:15CompleteDocTest2S1V13extendedFunc11x1yS2i_SitF</USR><Declaration>func extendedFunc1(x: Int, y: Int) -&gt; Int</Declaration><CommentParts><Abstract><Para>doc for extendedFunc1</Para></Abstract></CommentParts></Function>;
// MEMBER_NO_DOT-SAME: rawcomment=doc for extendedFunc1

// MEMBER_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal:   .extendedFunc2()[#Void#]; name=extendedFunc2()

// MEMBER_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal:   .instanceFunc1()[#Void#]; name=instanceFunc1();
// MEMBER_NO_DOT-SAME: briefcomment=struct instance func comment;
// MEMBER_NO_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="30" column="8"><Name>instanceFunc1()</Name><USR>s:15CompleteDocTest2S1V13instanceFunc1yyF</USR><Declaration>func instanceFunc1()</Declaration><CommentParts><Abstract><Para>struct instance func comment</Para></Abstract><Discussion><Para>Does stuff with the instance.</Para></Discussion></CommentParts></Function>;
// MEMBER_NO_DOT-SAME: rawcomment=struct instance func comment
// MEMBER_NO_DOT-EMPTY:
// MEMBER_NO_DOT-NEXT: Does stuff with the instance.

// MEMBER_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal:   .instanceFunc2({#x: String#})[#Void#]; name=instanceFunc2(x:);
// MEMBER_NO_DOT-SAME: briefcomment=struct instance func comment;
// MEMBER_NO_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="38" column="8"><Name>instanceFunc2(x:)</Name><USR>s:15CompleteDocTest2S1V13instanceFunc21xySS_tF</USR><Declaration>func instanceFunc2(x: String)</Declaration><CommentParts><Abstract><Para>struct instance func comment</Para></Abstract><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>what is x?</Para></Discussion></Parameter></Parameters><Discussion><Para>Does stuff with the instance.</Para></Discussion></CommentParts></Function>;
// MEMBER_NO_DOT-SAME: rawcomment=struct instance func comment
// MEMBER_NO_DOT-EMPTY:
// MEMBER_NO_DOT-NEXT: Does stuff with the instance.
// MEMBER_NO_DOT-EMPTY:
// MEMBER_NO_DOT-NEXT: - Parameters:
// MEMBER_NO_DOT-NEXT:   - x: what is x?

// MEMBER_NO_DOT-NEXT: Decl[InstanceVar]/CurrNominal:      .instanceVar1[#String#]; name=instanceVar1;
// MEMBER_NO_DOT-SAME: briefcomment=instanceVar1 description;
// MEMBER_NO_DOT-SAME: xmlcomment=<Other file="{{.*}}" line="20" column="7"><Name>instanceVar1</Name><USR>s:15CompleteDocTest2S1V12instanceVar1SSvp</USR><Declaration>var instanceVar1: String</Declaration><CommentParts><Abstract><Para>instanceVar1 description</Para></Abstract></CommentParts></Other>;
// MEMBER_NO_DOT-SAME: rawcomment=instanceVar1 description

// MEMBER_NO_DOT-NEXT: Decl[InstanceVar]/CurrNominal:      .instanceVar2[#Double#]; name=instanceVar2;
// MEMBER_NO_DOT-SAME: briefcomment=instanceVar2 description;
// MEMBER_NO_DOT-SAME: xmlcomment=<Other file="{{.*}}" line="25" column="7"><Name>instanceVar2</Name><USR>s:15CompleteDocTest2S1V12instanceVar2Sdvp</USR><Declaration>var instanceVar2: Double</Declaration><CommentParts><Abstract><Para>instanceVar2 description</Para></Abstract><Discussion><Para>Euler-Mascheroni constant</Para></Discussion></CommentParts></Other>;
// MEMBER_NO_DOT-SAME: rawcomment=instanceVar2 description
// MEMBER_NO_DOT-EMPTY:
// MEMBER_NO_DOT-NEXT: Euler-Mascheroni constant

// MEMBER_NO_DOT-NEXT: Decl[InstanceVar]/CurrNominal:      .requirement1[#String#]; name=requirement1;
// MEMBER_NO_DOT-SAME: briefcomment=requirement1 doc comment;
// MEMBER_NO_DOT-SAME: xmlcomment=<Other file="{{.*}}" line="7" column="7"><Name>requirement1</Name><USR>s:15CompleteDocTest2P1P12requirement1SSvp</USR><Declaration>var requirement1: String { get }</Declaration><CommentParts><Abstract><Para>requirement1 doc comment</Para></Abstract><Discussion><Para>more description</Para><Note><Para>This documentation comment was inherited from <codeVoice>P1</codeVoice>.</Para></Note></Discussion></CommentParts></Other>;
// MEMBER_NO_DOT-SAME: rawcomment=requirement1 doc comment
// MEMBER_NO_DOT-EMPTY:
// MEMBER_NO_DOT-NEXT: more description

// MEMBER_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal:   .requirement2()[#Void#]; name=requirement2()

// MEMBER_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal:   .requirement3()[#Void#]; name=requirement3();
// MEMBER_NO_DOT-SAME: briefcomment=requirement3 comment in S1;
// MEMBER_NO_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="51" column="8"><Name>requirement3()</Name><USR>s:15CompleteDocTest2S1V12requirement3yyF</USR><Declaration>func requirement3()</Declaration><CommentParts><Abstract><Para>requirement3 comment in S1</Para></Abstract></CommentParts></Function>;
// MEMBER_NO_DOT-SAME: rawcomment=requirement3 comment in S1

// MEMBER_NO_DOT-NEXT: Decl[InstanceMethod]/Super:         .requirement3({#x: Int#})[#Void#]; name=requirement3(x:);
// MEMBER_NO_DOT-SAME: briefcomment=requirement3 comment in P1;
// MEMBER_NO_DOT-SAME: xmlcomment=<Function file="{{.*}}" line="12" column="8"><Name>requirement3(x:)</Name><USR>s:15CompleteDocTest2P1P12requirement31xySi_tF</USR><Declaration>func requirement3(x: Int)</Declaration><CommentParts><Abstract><Para>requirement3 comment in P1</Para></Abstract></CommentParts></Function>;
// MEMBER_NO_DOT-SAME: rawcomment=requirement3 comment in P1

// MEMBER_NO_DOT-NEXT: Keyword[self]/CurrNominal:          .self[#S1#]; name=self
