// This is an input file for comment-to-{XML,Doxygen} conversion tests.
//
// Please keep this file in alphabetical order!

@objc class A000 {}
// CHECK:.swift:[[@LINE-1]]:13: Class/A000 {{.*}} FullCommentAsXML=none

/// Aaa.  A010.  Bbb.
@objc class A010_AttachToEntities {
// CHECK: swift:[[@LINE-1]]:13: Class/A010_AttachToEntities {{.*}} FullCommentAsXML=[<Other file="{{[^"]+}}swift" line="[[@LINE-1]]" column="13"><Name>A010_AttachToEntities</Name><USR>s:C14swift_ide_test21A010_AttachToEntities</USR><Abstract><Para>Aaa.  A010.  Bbb.</Para></Abstract></Other>]

  /// Aaa.  init().
  init() {}
// CHECK: swift:[[@LINE-1]]:3: Constructor/A010_AttachToEntities.init {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="3"><Name>init()</Name><USR>s:FC14swift_ide_test21A010_AttachToEntitiescFMS0_FT_S0_</USR><Abstract><Para>Aaa.  init().</Para></Abstract></Function>]

  /// Aaa.  subscript(i: Int).
  subscript(i: Int) -> Int {
// CHECK: swift:[[@LINE-1]]:3: Subscript/A010_AttachToEntities.subscript {{.*}} FullCommentAsXML=[<Other file="{{[^"]+}}swift" line="[[@LINE-1]]" column="3"><Name>subscript</Name><USR>s:sC14swift_ide_test21A010_AttachToEntities9subscriptFT1iSi_Si</USR><Abstract><Para>Aaa.  subscript(i: Int).</Para></Abstract></Other>]
    get {
// CHECK: swift:[[@LINE-1]]:5: Func/A010_AttachToEntities.<getter for A010_AttachToEntities.subscript> {{.*}} FullCommentAsXML=none
      return 0
    }
    set {}
// CHECK: swift:[[@LINE-1]]:5: Func/A010_AttachToEntities.<setter for A010_AttachToEntities.subscript> {{.*}} FullCommentAsXML=none
  }

  /// Aaa.  v1.
  var v1: Int = 0
// CHECK: swift:[[@LINE-1]]:7: Var/A010_AttachToEntities.v1 {{.*}} FullCommentAsXML=[<Other file="{{[^"]+}}swift" line="[[@LINE-1]]" column="7"><Name>v1</Name><USR>s:vC14swift_ide_test21A010_AttachToEntities2v1Si</USR><Abstract><Para>Aaa.  v1.</Para></Abstract></Other>]
}

@objc class A100_EmptyComments {
  ///
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f0FS0_FT_T_</USR></Function>]

  /// Aaa.
  func f1() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f1 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f1()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f1FS0_FT_T_</USR><Abstract><Para>Aaa.</Para></Abstract></Function>]

  /** */
  func f2() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f2 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f2()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f2FS0_FT_T_</USR></Function>]

  /**
   */
  func f3() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f3 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f3()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f3FS0_FT_T_</USR></Function>]

  /**
   * Aaa.
   */
  func f4() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A100_EmptyComments.f4 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f4()</Name><USR>s:FC14swift_ide_test18A100_EmptyComments2f4FS0_FT_T_</USR><Discussion><Para>Aaa.</Para></Discussion></Function>]
}

@objc class A110_Escaping {
  /// & < > " '
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A110_Escaping.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test13A110_Escaping2f0FS0_FT_T_</USR><Abstract><Para>&amp; &lt; &gt; &quot; &apos;</Para></Abstract></Function>]
}

@objc class A120_Brief {
  /// Aaa.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A120_Brief.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test10A120_Brief2f0FS0_FT_T_</USR><Abstract><Para>Aaa.</Para></Abstract></Function>]

  /// Aaa.
  ///
  /// Bbb.
  func f1() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A120_Brief.f1 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f1()</Name><USR>s:FC14swift_ide_test10A120_Brief2f1FS0_FT_T_</USR><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]

  ///Aaa.
  ///
  /// Bbb.
  func f2() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A120_Brief.f2 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f2()</Name><USR>s:FC14swift_ide_test10A120_Brief2f2FS0_FT_T_</USR><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]

  ///Aaa.
  ///
  ///Bbb.
  func f3() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A120_Brief.f3 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f3()</Name><USR>s:FC14swift_ide_test10A120_Brief2f3FS0_FT_T_</USR><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]
}

@objc class A200_ParamAndReturns {
  /// Aaa.  f0.
  ///
  /// :param: first Bbb.
  ///
  /// :param: second Ccc.  Ddd.
  ///   Eee.
  func f0(first: Int, second: Double) {}
// CHECK: swift:[[@LINE-1]]:8: Func/A200_ParamAndReturns.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0(first:second:)</Name><USR>s:FC14swift_ide_test20A200_ParamAndReturns2f0FS0_FT5firstSi6secondSd_T_</USR><Abstract><Para>Aaa. f0.</Para></Abstract><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>first Bbb.</Para></Discussion></Parameter><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>second Ccc. Ddd. Eee.</Para></Discussion></Parameter></Parameters></Function>]

  /// Aaa.  f1.
  ///
  /// :param: first Bbb.
  ///
  /// :returns: Ccc.
  ///   Ddd.
  func f1(first: Int) {}
// CHECK: swift:[[@LINE-1]]:8: Func/A200_ParamAndReturns.f1 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f1(first:)</Name><USR>s:FC14swift_ide_test20A200_ParamAndReturns2f1FS0_FT5firstSi_T_</USR><Abstract><Para>Aaa.  f1.</Para></Abstract><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>first Bbb.</Para></Discussion></Parameter></Parameters><ResultDiscussion><Para>Ccc. Ddd.</Para></ResultDiscussion></Function>]

  /// Aaa.  f2.
  ///
  /// :returns: Ccc.
  ///   Ddd.
  ///
  /// :returns: Eee.
  ///   Fff.
  func f2() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A200_ParamAndReturns.f2 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f2()</Name><USR>s:FC14swift_ide_test20A200_ParamAndReturns2f2FS0_FT_T_</USR><Abstract><Para>Aaa.  f2.</Para></Abstract><ResultDiscussion><Para>Ccc. Ddd.</Para><Para>Eee. Fff.</Para></ResultDiscussion></Function>]
}

@objc class A210_BulletList {
  /// * Aaa.
  ///
  /// * Bbb.
  ///   Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A210_BulletList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test15A210_BulletList2f0FS0_FT_T_</USR><Discussion><Para>Aaa.</Para><Para>Bbb. Ccc.</Para></Discussion></Function>]
}

@objc class A220_EnumeratedList {
  /// 1. Aaa.
  ///
  /// 2. Bbb.
  ///    Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A220_EnumeratedList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test19A220_EnumeratedList2f0FS0_FT_T_</USR><Discussion><Para>Aaa.</Para><Para>Bbb. Ccc.</Para></Discussion></Function>]
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
// CHECK: swift:[[@LINE-1]]:8: Func/A230_DefinitionList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test19A230_DefinitionList2f0FS0_FT_T_</USR><Discussion><Para>Aaa</Para><Para>Bbb.</Para><Para>Ccc</Para><Para>Ddd.</Para><Para>Eee : Fff</Para><Para>Ggg.</Para><Para>``Hhh``</Para><Para>Jjj.</Para></Discussion></Function>]
}

@objc class A240_FieldList {
  /// :unknown: Aaa.
  ///   Bbb.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A240_FieldList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test14A240_FieldList2f0FS0_FT_T_</USR><Discussion><Para>unknown</Para><Para>Aaa. Bbb.</Para></Discussion></Function>]

  /// * Aaa.
  ///
  ///   :param: Aaa.
  ///   :returns: Bbb.
  ///   :unknown: Ccc.
  func f1() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A240_FieldList.f1 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f1()</Name><USR>s:FC14swift_ide_test14A240_FieldList2f1FS0_FT_T_</USR><Discussion><Para>Aaa.</Para><Para>param</Para><Para>Aaa. :returns: Bbb. :unknown: Ccc.</Para></Discussion></Function>]
}

@objc class A250_OptionList {
  /// -a   Aaa.
  /// -b   Bbb.
  ///      Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A250_OptionList.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test15A250_OptionList2f0FS0_FT_T_</USR><Abstract><Para>-a   Aaa. -b   Bbb.</Para></Abstract><Discussion><Para>Ccc.</Para></Discussion></Function>]
}

@objc class A260_BlockQuote {
  /// Aaa.
  ///
  ///  Bbb.
  ///
  ///  Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A260_BlockQuote.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test15A260_BlockQuote2f0FS0_FT_T_</USR><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para><Para>Ccc.</Para></Discussion></Function>]
}

