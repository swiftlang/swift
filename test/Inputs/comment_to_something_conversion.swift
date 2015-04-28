// This is an input file for comment-to-{XML,Doxygen} conversion tests.
//
// Please keep this file in alphabetical order!

@objc class A000 {}
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  A010.  Bbb.
@objc class A010_AttachToEntities {
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>A010_AttachToEntities</Name><USR>s:C14swift_ide_test21A010_AttachToEntities</USR><Declaration>@objc class A010_AttachToEntities</Declaration><Abstract><Para>Aaa.  A010.  Bbb.</Para></Abstract></Class>]

/// Aaa.  init().
init() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>init()</Name><USR>s:FC14swift_ide_test21A010_AttachToEntitiescFMS0_FT_S0_</USR><Declaration>init()</Declaration><Abstract><Para>Aaa.  init().</Para></Abstract></Function>]

/// Aaa.  subscript(i: Int).
subscript(i: Int) -> Int {
// CHECK: {{.*}}DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>subscript(_:)</Name><USR>s:sC14swift_ide_test21A010_AttachToEntities9subscriptFSiSi</USR><Declaration>subscript (i: Int) -&gt; Int { get set }</Declaration><Abstract><Para>Aaa.  subscript(i: Int).</Para></Abstract></Other>]
    get {
// CHECK: {{.*}}DocCommentAsXML=none
      return 0
    }
    set {}
// CHECK: {{.*}}DocCommentAsXML=none
  }
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  v1.
var v1: Int = 0
// CHECK: {{.*}}DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>v1</Name><USR>s:vC14swift_ide_test21A010_AttachToEntities2v1Si</USR><Declaration>var v1: Int</Declaration><Abstract><Para>Aaa.  v1.</Para></Abstract></Other>]

/// Aaa.  v2.
class var v2: Int { return 0 }
// CHECK: {{.*}}DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>v2</Name><USR>s:ZvC14swift_ide_test21A010_AttachToEntities2v2Si</USR><Declaration>class var v2: Int { get }</Declaration><Abstract><Para>Aaa.  v2.</Para></Abstract></Other>]
// CHECK: {{.*}}DocCommentAsXML=none
}

/// Aaa.  A011.
struct A011_AttachToEntities {
}
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>A011_AttachToEntities</Name><USR>s:V14swift_ide_test21A011_AttachToEntities</USR><Declaration>struct A011_AttachToEntities</Declaration><Abstract><Para>Aaa.  A011.</Para></Abstract></Class>]

/// Aaa.  A012.
enum A012_AttachToEntities {
  case A
}
// CHECK: {{.*}}DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>A012_AttachToEntities</Name><USR>s:O14swift_ide_test21A012_AttachToEntities</USR><Declaration>enum A012_AttachToEntities</Declaration><Abstract><Para>Aaa.  A012.</Para></Abstract></Other>]
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  A013.
@objc protocol A013_AttachToEntities {}
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>A013_AttachToEntities</Name><USR>s:P14swift_ide_test21A013_AttachToEntities</USR><Declaration>@objc protocol A013_AttachToEntities</Declaration><Abstract><Para>Aaa.  A013.</Para></Abstract></Class>]

@objc class AutomaticLink {
// CHECK: {{.*}}DocCommentAsXML=none
  /// And now for a URL.
  ///
  /// <http://developer.apple.com/swift/>
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test13AutomaticLink2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>And now for a URL.</Para></Abstract><Discussion><Para><Link href="http://developer.apple.com/swift/">http://developer.apple.com/swift/</Link></Para></Discussion></Function>]
}

@objc class BlockQuote {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa.
  ///
  /// > Bbb.
  ///
  /// > Ccc.
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test10BlockQuote2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para><Para>Ccc.</Para></Discussion></Function>]
}

@objc class ATXHeaders {
// CHECK: {{.*}}DocCommentAsXML=none
  /// LEVEL ONE
  /// =========
  ///
  /// LEVEL TWO
  /// ---------
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test10ATXHeaders2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Discussion><rawHTML><![CDATA[<h1>]]></rawHTML>LEVEL ONE<rawHTML><![CDATA[</h1>]]></rawHTML><rawHTML><![CDATA[<h2>]]></rawHTML>LEVEL TWO<rawHTML><![CDATA[</h2>]]></rawHTML></Discussion></Function>]
}

@objc class Brief {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa.
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test5Brief2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]

  /// Aaa.
  ///
  /// Bbb.
  func f1() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1()</Name><USR>s:FC14swift_ide_test5Brief2f1FS0_FT_T_</USR><Declaration>func f1()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]

  ///Aaa.
  ///
  ///> Bbb.
  func f2() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f2()</Name><USR>s:FC14swift_ide_test5Brief2f2FS0_FT_T_</USR><Declaration>func f2()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]

  ///Aaa.
  ///
  ///Bbb.
  func f3() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f3()</Name><USR>s:FC14swift_ide_test5Brief2f3FS0_FT_T_</USR><Declaration>func f3()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]
}

@objc class CodeBlock {
// CHECK: {{.*}}DocCommentAsXML=none
  /// This is how you use this code.
  ///
  ///     f0() // WOW!
  ///     f0() // WOW!
  ///     f0() // WOW!
  func f0() {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test9CodeBlock2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>This is how you use this code.</Para></Abstract><Discussion><CodeListing><zCodeLineNumbered><![CDATA[f0() // WOW!]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[f0() // WOW!]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[f0() // WOW!]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing></Discussion></Function>]
}

@objc class EmptyComments {
// CHECK: {{.*}}DocCommentAsXML=none

  ///
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test13EmptyComments2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration></Function>]

  /// Aaa.
  func f1() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1()</Name><USR>s:FC14swift_ide_test13EmptyComments2f1FS0_FT_T_</USR><Declaration>func f1()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]

  /** */
  func f2() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f2()</Name><USR>s:FC14swift_ide_test13EmptyComments2f2FS0_FT_T_</USR><Declaration>func f2()</Declaration></Function>]

  /**
   */
  func f3() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f3()</Name><USR>s:FC14swift_ide_test13EmptyComments2f3FS0_FT_T_</USR><Declaration>func f3()</Declaration></Function>]

  /**
   * Aaa.
   */
  func f4() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f4()</Name><USR>s:FC14swift_ide_test13EmptyComments2f4FS0_FT_T_</USR><Declaration>func f4()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]
}

@objc class Emphasis {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa *bbb* ccc.
  /// Aaa _bbb_ ccc.
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test8Emphasis2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa <emphasis>bbb</emphasis> ccc. Aaa <emphasis>bbb</emphasis> ccc.</Para></Abstract></Function>]
}

@objc class HorizontalRules {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Briefly.
  ///
  /// ------------------------------------
  ///
  /// The end.
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test15HorizontalRules2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Briefly.</Para></Abstract><Discussion><rawHTML><![CDATA[<hr/>]]></rawHTML><Para>The end.</Para></Discussion></Function>]
}

@objc class ImplicitNameLink {
// CHECK: {{.*}}DocCommentAsXML=none
  /// [Apple][]
  ///
  /// [Apple]: https://www.apple.com/
  func f0() {}
}

@objc class InlineCode {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa `bbb` ccc.
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test10InlineCode2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa <codeVoice>bbb</codeVoice> ccc.</Para></Abstract></Function>]
}

@objc class InlineLink {
// CHECK: {{.*}}DocCommentAsXML=none
/// Aaa [bbb](/path/to/something) ccc.
func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test10InlineLink2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa <Link href="/path/to/something">bbb</Link> ccc.</Para></Abstract></Function>]
}

/// - parameter x: A number
@objc class ParamAndReturns {
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>ParamAndReturns</Name><USR>s:C14swift_ide_test15ParamAndReturns</USR><Declaration>@objc class ParamAndReturns</Declaration><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters></Class>]
/// Aaa.  f0.
///
/// - parameter first: Bbb.
///
/// - parameter second: Ccc.  Ddd.
///   Eee.
func f0(first: Int, second: Double) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0(_:second:)</Name><USR>s:FC14swift_ide_test15ParamAndReturns2f0FS0_FTSi6secondSd_T_</USR><Declaration>func f0(first: Int, second: Double)</Declaration><Abstract><Para>Aaa.  f0.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Bbb.</Para></Discussion></Parameter><Parameter><Name>second</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Ccc.  Ddd. Eee.</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  f1.
///
/// - parameter first: Bbb.
///
/// - returns: Ccc.
///   Ddd.
func f1(first: Int) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1(_:)</Name><USR>s:FC14swift_ide_test15ParamAndReturns2f1FS0_FSiT_</USR><Declaration>func f1(first: Int)</Declaration><Abstract><Para>Aaa.  f1.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Bbb.</Para></Discussion></Parameter></Parameters><ResultDiscussion><Para>Ccc. Ddd.</Para></ResultDiscussion></Function>]
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  f2.
///
/// - parameter first:
///
/// - parameter second: Aaa.
///
/// - parameter third:
///   Bbb.
func f2(first: Int, second: Double, third: Float) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f2(_:second:third:)</Name><USR>s:FC14swift_ide_test15ParamAndReturns2f2FS0_FTSi6secondSd5thirdSf_T_</USR><Declaration>func f2(first: Int, second: Double, third: Float)</Declaration><Abstract><Para>Aaa.  f2.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para></Para></Discussion></Parameter><Parameter><Name>second</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Aaa.</Para></Discussion></Parameter><Parameter><Name>third</Name><Direction isExplicit="0">in</Direction><Discussion><Para> Bbb.</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  f3.
///
/// - parameter first: Bbb.
/// - parameter second: Ccc.
/// - parameter third: Ddd.
func f3(first: Int, second: Double, third: Float) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f3(_:second:third:)</Name><USR>s:FC14swift_ide_test15ParamAndReturns2f3FS0_FTSi6secondSd5thirdSf_T_</USR><Declaration>func f3(first: Int, second: Double, third: Float)</Declaration><Abstract><Para>Aaa.  f3.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Bbb.</Para></Discussion></Parameter><Parameter><Name>second</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Ccc.</Para></Discussion></Parameter><Parameter><Name>third</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Ddd.</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none


/// Aaa.  f4.
///
/// - returns: Ccc.
///   Ddd.
///
/// - returns: Eee.
///   Fff.
func f4() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f4()</Name><USR>s:FC14swift_ide_test15ParamAndReturns2f4FS0_FT_T_</USR><Declaration>func f4()</Declaration><Abstract><Para>Aaa.  f4.</Para></Abstract><ResultDiscussion><Para>Eee. Fff.</Para></ResultDiscussion></Function>]
}

@objc class OrderedList {
// CHECK: {{.*}}DocCommentAsXML=none
/// 1. Aaa.
///
/// 2. Bbb.
///    Ccc.
func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test11OrderedList2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Discussion><List-Number><Item><Para>Aaa.</Para></Item><Item><Para>Bbb. Ccc.</Para></Item></List-Number></Discussion></Function>]
}

@objc class ParameterOutline{
// CHECK: {{.*}}DocCommentAsXML=none
/// - Parameters:
///   - x: A number
///   - y: A number
///
/// - PARAMETERS:
///   - z: A number
func f0(x: Int, y: Int, z: Int) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0(_:y:z:)</Name><USR>s:FC14swift_ide_test16ParameterOutline2f0FS0_FTSi1ySi1zSi_T_</USR><Declaration>func f0(x: Int, y: Int, z: Int)</Declaration><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter><Parameter><Name>y</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter><Parameter><Name>z</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
}

@objc class ParameterOutlineMiddle {
// CHECK: {{.*}}DocCommentAsXML=none
/// - This line should remain.
/// - Parameters:
///   - x: A number
///   - y: A number
/// - This line should also remain.
/// - parameter z: A number
func f0(x: Int, y: Int, z: Int) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0(_:y:z:)</Name><USR>s:FC14swift_ide_test22ParameterOutlineMiddle2f0FS0_FTSi1ySi1zSi_T_</USR><Declaration>func f0(x: Int, y: Int, z: Int)</Declaration><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter><Parameter><Name>y</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter><Parameter><Name>z</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters><Discussion><List-Bullet><Item><Para>This line should remain.</Para></Item><Item><Para>This line should also remain.</Para></Item></List-Bullet></Discussion></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
}

@objc class ReferenceLink {
// CHECK: {{.*}}DocCommentAsXML=none
  /// This is [a reference link] [1].
  ///
  /// [1]: http://developer.apple.com/
}


@objc class Returns {
// CHECK: {{.*}}DocCommentAsXML=none
  /// - returns: A number
  func f0() -> Int {
    return 0
  }
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test7Returns2f0FS0_FT_Si</USR><Declaration>func f0() -&gt; Int</Declaration><ResultDiscussion><Para>A number</Para></ResultDiscussion></Function>]
}

@objc class SeparateParameters {
// CHECK: {{.*}}DocCommentAsXML=none
  /// - Parameter x: A number
  func f0(x: Int, y: Int) {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0(_:y:)</Name><USR>s:FC14swift_ide_test18SeparateParameters2f0FS0_FTSi1ySi_T_</USR><Declaration>func f0(x: Int, y: Int)</Declaration><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
}

@objc class SetextHeaders {
// CHECK: {{.*}}DocCommentAsXML=none
  /// # LEVEL ONE
  ///
  /// ## LEVEL TWO
  ///
  /// ### LEVEL THREE
  ///
  /// #### LEVEL FOUR
  ///
  /// ##### LEVEL FIVE
  ///
  /// ##### LEVEL SIX
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test13SetextHeaders2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Discussion><rawHTML><![CDATA[<h1>]]></rawHTML>LEVEL ONE<rawHTML><![CDATA[</h1>]]></rawHTML><rawHTML><![CDATA[<h2>]]></rawHTML>LEVEL TWO<rawHTML><![CDATA[</h2>]]></rawHTML><rawHTML><![CDATA[<h3>]]></rawHTML>LEVEL THREE<rawHTML><![CDATA[</h3>]]></rawHTML><rawHTML><![CDATA[<h4>]]></rawHTML>LEVEL FOUR<rawHTML><![CDATA[</h4>]]></rawHTML><rawHTML><![CDATA[<h5>]]></rawHTML>LEVEL FIVE<rawHTML><![CDATA[</h5>]]></rawHTML><rawHTML><![CDATA[<h5>]]></rawHTML>LEVEL SIX<rawHTML><![CDATA[</h5>]]></rawHTML></Discussion></Function>]
}

@objc class StrongEmphasis {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa **bbb** ccc.
  /// Aaa __bbb__ ccc.
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test14StrongEmphasis2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa <bold>bbb</bold> ccc. Aaa <bold>bbb</bold> ccc.</Para></Abstract></Function>]
}

@objc class UnorderedList {
// CHECK: {{.*}}DocCommentAsXML=none
  /// * Aaa.
  ///
  /// * Bbb.
  ///   Ccc.
  ///
  /// - Ddd.
  /// - Eee.
  ///   - Fff.
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test13UnorderedList2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Discussion><List-Bullet><Item><Para>Aaa.</Para></Item><Item><Para>Bbb. Ccc.</Para></Item></List-Bullet><List-Bullet><Item><Para>Ddd.</Para></Item><Item><Para>Eee.</Para><List-Bullet><Item><Para>Fff.</Para></Item></List-Bullet></Item></List-Bullet></Discussion></Function>]
}

@objc class IndentedBlockComment {
// CHECK: {{.*}}DocCommentAsXML=none
  /**
      Brief.

      First paragraph line.
      Second paragraph line.

      Now for a code sample:

          var x = 1
          // var y = 2
          var z = 3
  */
  func f1() {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1()</Name><USR>s:FC14swift_ide_test20IndentedBlockComment2f1FS0_FT_T_</USR><Declaration>func f1()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>First paragraph line. Second paragraph line.</Para><Para>Now for a code sample:</Para><CodeListing><zCodeLineNumbered><![CDATA[var x = 1]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// var y = 2]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[var z = 3]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing></Discussion></Function>]
  /**
                        Hugely indented brief.

                        First paragraph line.
                        Second paragraph line.

                        Now for a code sample:

                            var x = 1
                            // var y = 2
                            var z = 3
  */
  func f2() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f2()</Name><USR>s:FC14swift_ide_test20IndentedBlockComment2f2FS0_FT_T_</USR><Declaration>func f2()</Declaration><Abstract><Para>Hugely indented brief.</Para></Abstract><Discussion><Para>First paragraph line. Second paragraph line.</Para><Para>Now for a code sample:</Para><CodeListing><zCodeLineNumbered><![CDATA[var x = 1]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// var y = 2]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[var z = 3]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing></Discussion></Function>]
}

@objc class MultiLineBrief {
// CHECK: {{.*}}DocCommentAsXML=none

  /// Brief first line.
  /// Brief after softbreak.
  ///
  /// Some paragraph text.
  func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:FC14swift_ide_test14MultiLineBrief2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Brief first line. Brief after softbreak.</Para></Abstract><Discussion><Para>Some paragraph text.</Para></Discussion></Function>]
}

