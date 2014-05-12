// This is an input file for comment-to-{XML,Doxygen} conversion tests.
//
// Please keep this file in alphabetical order!

@objc class A000 {}
// CHECK:.swift:[[@LINE-1]]:13: Class/A000 {{.*}} FullCommentAsXML=none

/// Aaa.  A010.  Bbb.
@objc class A010_AttachToEntities {
// CHECK: swift:[[@LINE-1]]:13: Class/A010_AttachToEntities {{.*}} FullCommentAsXML=[<Class file="{{[^"]+}}swift" line="[[@LINE-1]]" column="13"><Name>A010_AttachToEntities</Name><USR>s:C14swift_ide_test21A010_AttachToEntities</USR><Declaration>@objc class A010_AttachToEntities</Declaration><Abstract><Para>Aaa.  A010.  Bbb.</Para></Abstract></Class>]

  /// Aaa.  init().
  init() {}
// CHECK: swift:[[@LINE-1]]:3: Constructor/A010_AttachToEntities.init {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="3"><Name>init()</Name><USR>s:FC14swift_ide_test21A010_AttachToEntitiescFMS0_FT_S0_</USR><Declaration>init()</Declaration><Abstract><Para>Aaa.  init().</Para></Abstract></Function>]

  /// Aaa.  subscript(i: Int).
  subscript(i: Int) -> Int {
// CHECK: swift:[[@LINE-1]]:3: Subscript/A010_AttachToEntities.subscript {{.*}} FullCommentAsXML=[<Other file="{{[^"]+}}swift" line="[[@LINE-1]]" column="3"><Name>subscript(_:)</Name><USR>s:sC14swift_ide_test21A010_AttachToEntities9subscriptFSiSi</USR><Declaration>subscript (i: Int) -> Int { get set }</Declaration><Abstract><Para>Aaa.  subscript(i: Int).</Para></Abstract></Other>]
    get {
// CHECK: swift:[[@LINE-1]]:5: Func/A010_AttachToEntities.<getter for A010_AttachToEntities.subscript> {{.*}} FullCommentAsXML=none
      return 0
    }
    set {}
// CHECK: swift:[[@LINE-1]]:5: Func/A010_AttachToEntities.<setter for A010_AttachToEntities.subscript> {{.*}} FullCommentAsXML=none
  }

  /// Aaa.  v1.
  var v1: Int = 0
// CHECK: swift:[[@LINE-1]]:7: Var/A010_AttachToEntities.v1 {{.*}} FullCommentAsXML=[<Other file="{{[^"]+}}swift" line="[[@LINE-1]]" column="7"><Name>v1</Name><USR>s:vC14swift_ide_test21A010_AttachToEntities2v1Si</USR><Declaration>var v1: Int</Declaration><Abstract><Para>Aaa.  v1.</Para></Abstract></Other>]
}

/// Aaa.  A011.
struct A011_AttachToEntities {
// CHECK: swift:[[@LINE-1]]:8: Struct/A011_AttachToEntities {{.*}} FullCommentAsXML=[<Class file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>A011_AttachToEntities</Name><USR>s:V14swift_ide_test21A011_AttachToEntities</USR><Declaration>struct A011_AttachToEntities</Declaration><Abstract><Para>Aaa.  A011.</Para></Abstract></Class>]
}

/// Aaa.  A012.
enum A012_AttachToEntities {
// CHECK: swift:[[@LINE-1]]:6: Enum/A012_AttachToEntities {{.*}} FullCommentAsXML=[<Other file="{{[^"]+}}swift" line="[[@LINE-1]]" column="6"><Name>A012_AttachToEntities</Name><USR>s:O14swift_ide_test21A012_AttachToEntities</USR><Declaration>enum A012_AttachToEntities</Declaration><Abstract><Para>Aaa.  A012.</Para></Abstract></Other>]
  case A
}

/// Aaa.  A013.
@objc protocol A013_AttachToEntities {
// CHECK: swift:[[@LINE-1]]:16: Protocol/A013_AttachToEntities {{.*}} FullCommentAsXML=[<Class file="{{[^"]+}}swift" line="[[@LINE-1]]" column="16"><Name>A013_AttachToEntities</Name><USR>s:P14swift_ide_test21A013_AttachToEntities</USR><Declaration>@objc protocol A013_AttachToEntities</Declaration><Abstract><Para>Aaa.  A013.</Para></Abstract></Class>]
}

@objc class A100_EmptyComments {
  ///
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration></Function>]

  /// Aaa.
  func f1() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f1 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f1()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f1FS0_FT_T_</USR><Declaration>func f1()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]

  /** */
  func f2() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f2 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f2()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f2FS0_FT_T_</USR><Declaration>func f2()</Declaration></Function>]

  /**
   */
  func f3() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f3 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f3()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f3FS0_FT_T_</USR><Declaration>func f3()</Declaration></Function>]

  /**
   * Aaa.
   */
  func f4() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f4 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f4()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f4FS0_FT_T_</USR><Declaration>func f4()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]
}

@objc class A110_Escaping {
  /// & < > " '
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A110_Escaping.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test13A110_Escaping2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>&amp; &lt; &gt; &quot; &apos;</Para></Abstract></Function>]
}

@objc class A120_Brief {
  /// Aaa.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A120_Brief.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test10A120_Brief2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]

  /// Aaa.
  ///
  /// Bbb.
  func f1() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A120_Brief.f1 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f1()</Name><USR>s:FC14swift_ide_test10A120_Brief2f1FS0_FT_T_</USR><Declaration>func f1()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]

  ///Aaa.
  ///
  /// Bbb.
  func f2() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A120_Brief.f2 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f2()</Name><USR>s:FC14swift_ide_test10A120_Brief2f2FS0_FT_T_</USR><Declaration>func f2()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para><rawHTML><![CDATA[<blockquote>]]></rawHTML>Bbb.<rawHTML><![CDATA[</blockquote>]]></rawHTML></Para></Discussion></Function>]

  ///Aaa.
  ///
  ///Bbb.
  func f3() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A120_Brief.f3 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f3()</Name><USR>s:FC14swift_ide_test10A120_Brief2f3FS0_FT_T_</USR><Declaration>func f3()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]
}

@objc class A200_ParamAndReturns {
  /// Aaa.  f0.
  ///
  /// :param: first Bbb.
  ///
  /// :param: second Ccc.  Ddd.
  ///   Eee.
  func f0(first: Int, second: Double) {}
// CHECK: swift:[[@LINE-1]]:8: Func/A200_ParamAndReturns.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0(_:second:)</Name><USR>s:FC14swift_ide_test20A200_ParamAndReturns2f0FS0_FTSi6secondSd_T_</USR><Declaration>func f0(first: Int, second: Double)</Declaration><Abstract><Para>Aaa. f0.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Bbb.</Para></Discussion></Parameter><Parameter><Name>second</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Ccc. Ddd. Eee.</Para></Discussion></Parameter></Parameters></Function>]

  /// Aaa.  f1.
  ///
  /// :param: first Bbb.
  ///
  /// :returns: Ccc.
  ///   Ddd.
  func f1(first: Int) {}
// CHECK: swift:[[@LINE-1]]:8: Func/A200_ParamAndReturns.f1 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f1(_:)</Name><USR>s:FC14swift_ide_test20A200_ParamAndReturns2f1FS0_FSiT_</USR><Declaration>func f1(first: Int)</Declaration><Abstract><Para>Aaa.  f1.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Bbb.</Para></Discussion></Parameter></Parameters><ResultDiscussion><Para>Ccc. Ddd.</Para></ResultDiscussion></Function>]

  /// Aaa.  f2.
  ///
  /// :param: first
  ///
  /// :param:
  ///           second
  ///
  /// :param:
  ///           third
  ///           Bbb.
  func f2(first: Int, second: Double, third: Float) {}
// CHECK: swift:[[@LINE-1]]:8: Func/A200_ParamAndReturns.f2 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f2(_:second:third:)</Name><USR>s:FC14swift_ide_test20A200_ParamAndReturns2f2FS0_FTSi6secondSd5thirdSf_T_</USR><Declaration>func f2(first: Int, second: Double, third: Float)</Declaration><Abstract><Para>Aaa.  f2.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para></Para></Discussion></Parameter><Parameter><Name>second</Name><Direction isExplicit="0">in</Direction><Discussion><Para></Para></Discussion></Parameter><Parameter><Name>third</Name><Direction isExplicit="0">in</Direction><Discussion><Para> Bbb.</Para></Discussion></Parameter></Parameters></Function>]

  /// Aaa.  f3.
  ///
  /// :returns: Ccc.
  ///   Ddd.
  ///
  /// :returns: Eee.
  ///   Fff.
  func f3() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A200_ParamAndReturns.f3 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f3()</Name><USR>s:FC14swift_ide_test20A200_ParamAndReturns2f3FS0_FT_T_</USR><Declaration>func f3()</Declaration><Abstract><Para>Aaa.  f3.</Para></Abstract><ResultDiscussion><Para>Ccc. Ddd.</Para><Para>Eee. Fff.</Para></ResultDiscussion></Function>]
}

@objc class A210_BulletList {
  /// * Aaa.
  ///
  /// * Bbb.
  ///   Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A210_BulletList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test15A210_BulletList2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Discussion><Para><rawHTML><![CDATA[<ul>]]></rawHTML><rawHTML><![CDATA[<li>]]></rawHTML>Aaa.<rawHTML><![CDATA[</li>]]></rawHTML><rawHTML><![CDATA[<li>]]></rawHTML>Bbb. Ccc.<rawHTML><![CDATA[</li>]]></rawHTML><rawHTML><![CDATA[</ul>]]></rawHTML></Para></Discussion></Function>]
}

@objc class A220_EnumeratedList {
  /// 1. Aaa.
  ///
  /// 2. Bbb.
  ///    Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A220_EnumeratedList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test19A220_EnumeratedList2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Discussion><Para><rawHTML><![CDATA[<ol>]]></rawHTML><rawHTML><![CDATA[<li>]]></rawHTML>Aaa.<rawHTML><![CDATA[</li>]]></rawHTML><rawHTML><![CDATA[<li>]]></rawHTML>Bbb. Ccc.<rawHTML><![CDATA[</li>]]></rawHTML><rawHTML><![CDATA[</ol>]]></rawHTML></Para></Discussion></Function>]
}

@objc class A230_DefinitionList {
  /// Aaa
  ///   Bbb.
  ///
  /// Ccc
  ///   Ddd.
  ///
  /// Eee : Fff
  ///   Ggg.
  ///
  /// ``Hhh``
  ///   Jjj.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A230_DefinitionList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test19A230_DefinitionList2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Discussion><Para><rawHTML><![CDATA[<dl>]]></rawHTML><rawHTML><![CDATA[<dt>]]></rawHTML>Aaa<rawHTML><![CDATA[</dt>]]></rawHTML><rawHTML><![CDATA[<dd>]]></rawHTML>Bbb.<rawHTML><![CDATA[</dd>]]></rawHTML><rawHTML><![CDATA[<dt>]]></rawHTML>Ccc<rawHTML><![CDATA[</dt>]]></rawHTML><rawHTML><![CDATA[<dd>]]></rawHTML>Ddd.<rawHTML><![CDATA[</dd>]]></rawHTML><rawHTML><![CDATA[<dt>]]></rawHTML>Eee : Fff<rawHTML><![CDATA[</dt>]]></rawHTML><rawHTML><![CDATA[<dd>]]></rawHTML>Ggg.<rawHTML><![CDATA[</dd>]]></rawHTML><rawHTML><![CDATA[<dt>]]></rawHTML>``Hhh``<rawHTML><![CDATA[</dt>]]></rawHTML><rawHTML><![CDATA[<dd>]]></rawHTML>Jjj.<rawHTML><![CDATA[</dd>]]></rawHTML><rawHTML><![CDATA[</dl>]]></rawHTML></Para></Discussion></Function>]
}

@objc class A240_FieldList {
  /// :unknown: Aaa.
  ///   Bbb.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A240_FieldList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test14A240_FieldList2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Discussion><Para><rawHTML><![CDATA[<dl>]]></rawHTML><rawHTML><![CDATA[<dt>]]></rawHTML>unknown<rawHTML><![CDATA[</dt>]]></rawHTML><rawHTML><![CDATA[<dd>]]></rawHTML>Aaa. Bbb.<rawHTML><![CDATA[</dd>]]></rawHTML><rawHTML><![CDATA[</dl>]]></rawHTML></Para></Discussion></Function>]

  /// * Aaa.
  ///
  ///   :param: Aaa.
  ///
  ///   :returns: Bbb.
  ///
  ///   :unknown: Ccc.
  func f1() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A240_FieldList.f1 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f1()</Name><USR>s:FC14swift_ide_test14A240_FieldList2f1FS0_FT_T_</USR><Declaration>func f1()</Declaration><Discussion><Para><rawHTML><![CDATA[<ul>]]></rawHTML><rawHTML><![CDATA[<li>]]></rawHTML>Aaa.<rawHTML><![CDATA[<dl>]]></rawHTML><rawHTML><![CDATA[<dt>]]></rawHTML>param<rawHTML><![CDATA[</dt>]]></rawHTML><rawHTML><![CDATA[<dd>]]></rawHTML>Aaa.<rawHTML><![CDATA[</dd>]]></rawHTML><rawHTML><![CDATA[<dt>]]></rawHTML>returns<rawHTML><![CDATA[</dt>]]></rawHTML><rawHTML><![CDATA[<dd>]]></rawHTML>Bbb.<rawHTML><![CDATA[</dd>]]></rawHTML><rawHTML><![CDATA[<dt>]]></rawHTML>unknown<rawHTML><![CDATA[</dt>]]></rawHTML><rawHTML><![CDATA[<dd>]]></rawHTML>Ccc.<rawHTML><![CDATA[</dd>]]></rawHTML><rawHTML><![CDATA[</dl>]]></rawHTML><rawHTML><![CDATA[</li>]]></rawHTML><rawHTML><![CDATA[</ul>]]></rawHTML></Para></Discussion></Function>]
}

@objc class A250_OptionList {
  /// -a   Aaa.
  /// -b   Bbb.
  ///      Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A250_OptionList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test15A250_OptionList2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>-a   Aaa. -b   Bbb.</Para></Abstract><Discussion><Para><rawHTML><![CDATA[<blockquote>]]></rawHTML>Ccc.<rawHTML><![CDATA[</blockquote>]]></rawHTML></Para></Discussion></Function>]
}

@objc class A260_BlockQuote {
  /// Aaa.
  ///
  ///  Bbb.
  ///
  ///  Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A260_BlockQuote.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test15A260_BlockQuote2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para><rawHTML><![CDATA[<blockquote>]]></rawHTML>Bbb.Ccc.<rawHTML><![CDATA[</blockquote>]]></rawHTML></Para></Discussion></Function>]
}

