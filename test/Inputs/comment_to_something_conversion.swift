// This is an input file for comment-to-{XML,Doxygen} conversion tests.
//
// Please keep this file in alphabetical order!

@objc public class A000 {}
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  A010.  Bbb.
@objc public class A010_AttachToEntities {
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>A010_AttachToEntities</Name><USR>s:14swift_ide_test21A010_AttachToEntitiesC</USR><Declaration>@objc public class A010_AttachToEntities</Declaration><Abstract><Para>Aaa.  A010.  Bbb.</Para></Abstract></Class>]

/// Aaa.  init().
public init() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>init()</Name><USR>s:14swift_ide_test21A010_AttachToEntitiesCACycfc</USR><Declaration>public init()</Declaration><Abstract><Para>Aaa.  init().</Para></Abstract></Function>]

/// Aaa.  subscript(i: Int).
public subscript(i: Int) -> Int {
// CHECK: {{.*}}DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>subscript(_:)</Name><USR>s:14swift_ide_test21A010_AttachToEntitiesC9subscriptSiSici</USR><Declaration>public subscript(i: Int) -&gt; Int { get set }</Declaration><Abstract><Para>Aaa.  subscript(i: Int).</Para></Abstract></Other>]
    get {
// CHECK: {{.*}}DocCommentAsXML=none
      return 0
    }
    set {}
// CHECK: {{.*}}DocCommentAsXML=none
  }
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  v1.
public var v1: Int = 0
// CHECK: {{.*}}DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>v1</Name><USR>s:14swift_ide_test21A010_AttachToEntitiesC2v1Siv</USR><Declaration>public var v1: Int</Declaration><Abstract><Para>Aaa.  v1.</Para></Abstract></Other>]

/// Aaa.  v2.
public class var v2: Int { return 0 }
// CHECK: {{.*}}DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>v2</Name><USR>s:14swift_ide_test21A010_AttachToEntitiesC2v2SivZ</USR><Declaration>public class var v2: Int { get }</Declaration><Abstract><Para>Aaa.  v2.</Para></Abstract></Other>]
// CHECK: {{.*}}DocCommentAsXML=none
}

/// Aaa.  A011.
public struct A011_AttachToEntities {
}
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>A011_AttachToEntities</Name><USR>s:14swift_ide_test21A011_AttachToEntitiesV</USR><Declaration>public struct A011_AttachToEntities</Declaration><Abstract><Para>Aaa.  A011.</Para></Abstract></Class>]

/// Aaa.  A012.
public enum A012_AttachToEntities {
  case A
}
// CHECK: {{.*}}DocCommentAsXML=[<Other file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>A012_AttachToEntities</Name><USR>s:14swift_ide_test21A012_AttachToEntitiesO</USR><Declaration>public enum A012_AttachToEntities</Declaration><Abstract><Para>Aaa.  A012.</Para></Abstract></Other>]
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  A013.
@objc public protocol A013_AttachToEntities {}
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>A013_AttachToEntities</Name><USR>s:14swift_ide_test21A013_AttachToEntitiesP</USR><Declaration>@objc public protocol A013_AttachToEntities</Declaration><Abstract><Para>Aaa.  A013.</Para></Abstract></Class>]

@objc public class ATXHeaders {
// CHECK: {{.*}}DocCommentAsXML=none
  /// LEVEL ONE
  /// =========
  ///
  /// LEVEL TWO
  /// ---------
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test10ATXHeadersC2f0yyF</USR><Declaration>public func f0()</Declaration><Discussion><rawHTML><![CDATA[<h1>]]></rawHTML>LEVEL ONE<rawHTML><![CDATA[</h1>]]></rawHTML><rawHTML><![CDATA[<h2>]]></rawHTML>LEVEL TWO<rawHTML><![CDATA[</h2>]]></rawHTML></Discussion></Function>]
}

@objc public class AutomaticLink {
// CHECK: {{.*}}DocCommentAsXML=none
  /// And now for a URL.
  ///
  /// <http://developer.apple.com/swift/>
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test13AutomaticLinkC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>And now for a URL.</Para></Abstract><Discussion><Para><Link href="http://developer.apple.com/swift/">http://developer.apple.com/swift/</Link></Para></Discussion></Function>]
}

@objc public class BlockQuote {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa.
  ///
  /// > Bbb.
  ///
  /// > Ccc.
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test10BlockQuoteC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para><Para>Ccc.</Para></Discussion></Function>]
}

@objc public class Brief {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa.
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test5BriefC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]

  /// Aaa.
  ///
  /// Bbb.
  public func f1() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1()</Name><USR>s:14swift_ide_test5BriefC2f1yyF</USR><Declaration>public func f1()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]

  /// Aaa.
  ///
  ///> Bbb.
  public func f2() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f2()</Name><USR>s:14swift_ide_test5BriefC2f2yyF</USR><Declaration>public func f2()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]

  /// Aaa.
  ///
  /// Bbb.
  public func f3() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f3()</Name><USR>s:14swift_ide_test5BriefC2f3yyF</USR><Declaration>public func f3()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Para>Bbb.</Para></Discussion></Function>]
}

@objc public class ClosingComments {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Some comment. */
  public func closingComment() {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>closingComment()</Name><USR>s:14swift_ide_test15ClosingCommentsC14closingCommentyyF</USR><Declaration>public func closingComment()</Declaration><Abstract><Para>Some comment. */</Para></Abstract></Function>]
}

@objc public class ClosureContainer {
/// Partially applies a binary operator.
///
/// - Parameter a: The left-hand side to partially apply.
/// - Parameter combine: A binary operator.
///   - Parameter lhs: The left-hand side of the operator
///   - Parameter rhs: The right-hand side of the operator
///   - Returns: A result.
///   - Throws: Nothing.
@objc public func closureParameterExplodedExploded(a: Int, combine: (_ lhs: Int, _ rhs: Int) -> Int) {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>closureParameterExplodedExploded(a:combine:)</Name><USR>s:14swift_ide_test16ClosureContainerC024closureParameterExplodedH0ySi1a_SiSi_Sitc7combinetF</USR><Declaration>@objc public func closureParameterExplodedExploded(a: Int, combine: (_ lhs: Int, _ rhs: Int) -&gt; Int)</Declaration><Abstract><Para>Partially applies a binary operator.</Para></Abstract><Parameters><Parameter><Name>a</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The left-hand side to partially apply.</Para></Discussion></Parameter><Parameter><Name>combine</Name><Direction isExplicit="0">in</Direction><ClosureParameter><Abstract><Para>A binary operator.</Para></Abstract><Parameters><Parameter><Name>lhs</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The left-hand side of the operator</Para></Discussion></Parameter><Parameter><Name>rhs</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The right-hand side of the operator</Para></Discussion></Parameter></Parameters><ResultDiscussion><Para>A result.</Para></ResultDiscussion><ThrowsDiscussion><Para>Nothing.</Para></ThrowsDiscussion></ClosureParameter></Parameter></Parameters></Function>]

/// Partially applies a binary operator.
///
/// - Parameters:
///   - a: The left-hand side to partially apply.
///   - combine: A binary operator.
///     - Parameter lhs: The left-hand side of the operator
///     - Parameter rhs: The right-hand side of the operator
///     - Returns: A result.
///     - Throws: Nothing.
@objc public func closureParameterOutlineExploded(a: Int, combine: (_ lhs: Int, _ rhs: Int) -> Int) {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>closureParameterOutlineExploded(a:combine:)</Name><USR>s:14swift_ide_test16ClosureContainerC31closureParameterOutlineExplodedySi1a_SiSi_Sitc7combinetF</USR><Declaration>@objc public func closureParameterOutlineExploded(a: Int, combine: (_ lhs: Int, _ rhs: Int) -&gt; Int)</Declaration><Abstract><Para>Partially applies a binary operator.</Para></Abstract><Parameters><Parameter><Name>a</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The left-hand side to partially apply.</Para></Discussion></Parameter><Parameter><Name>combine</Name><Direction isExplicit="0">in</Direction><ClosureParameter><Abstract><Para>A binary operator.</Para></Abstract><Parameters><Parameter><Name>lhs</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The left-hand side of the operator</Para></Discussion></Parameter><Parameter><Name>rhs</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The right-hand side of the operator</Para></Discussion></Parameter></Parameters><ResultDiscussion><Para>A result.</Para></ResultDiscussion><ThrowsDiscussion><Para>Nothing.</Para></ThrowsDiscussion></ClosureParameter></Parameter></Parameters></Function>]

/// Partially applies a binary operator.
///
/// - Parameters:
///   - a: The left-hand side to partially apply.
///   - combine: A binary operator.
///     - Parameters:
///       - lhs: The left-hand side of the operator
///       - rhs: The right-hand side of the operator
///     - Returns: A result.
///     - Throws: Nothing.
@objc public func closureParameterOutlineOutline(a: Int, combine: (_ lhs: Int, _ rhs: Int) -> Int) {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>closureParameterOutlineOutline(a:combine:)</Name><USR>s:14swift_ide_test16ClosureContainerC023closureParameterOutlineH0ySi1a_SiSi_Sitc7combinetF</USR><Declaration>@objc public func closureParameterOutlineOutline(a: Int, combine: (_ lhs: Int, _ rhs: Int) -&gt; Int)</Declaration><Abstract><Para>Partially applies a binary operator.</Para></Abstract><Parameters><Parameter><Name>a</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The left-hand side to partially apply.</Para></Discussion></Parameter><Parameter><Name>combine</Name><Direction isExplicit="0">in</Direction><ClosureParameter><Abstract><Para>A binary operator.</Para></Abstract><Parameters><Parameter><Name>lhs</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The left-hand side of the operator</Para></Discussion></Parameter><Parameter><Name>rhs</Name><Direction isExplicit="0">in</Direction><Discussion><Para>The right-hand side of the operator</Para></Discussion></Parameter></Parameters><ResultDiscussion><Para>A result.</Para></ResultDiscussion><ThrowsDiscussion><Para>Nothing.</Para></ThrowsDiscussion></ClosureParameter></Parameter></Parameters></Function>]
}

@objc public class CodeBlock {
// CHECK: {{.*}}DocCommentAsXML=none
  /// This is how you use this code.
  ///
  ///     f0() // WOW!
  ///     f0() // WOW!
  ///     f0() // WOW!
  public func f0() {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test9CodeBlockC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>This is how you use this code.</Para></Abstract><Discussion><CodeListing language="swift"><zCodeLineNumbered><![CDATA[f0() // WOW!]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[f0() // WOW!]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[f0() // WOW!]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing></Discussion></Function>]
}

@objc public class Emphasis {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa *bbb* ccc.
  /// Aaa _bbb_ ccc.
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test8EmphasisC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>Aaa <emphasis>bbb</emphasis> ccc. Aaa <emphasis>bbb</emphasis> ccc.</Para></Abstract></Function>]
}

@objc public class EmptyComments {
// CHECK: {{.*}}DocCommentAsXML=none

  ///
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test13EmptyCommentsC2f0yyF</USR><Declaration>public func f0()</Declaration></Function>]

  /// Aaa.
  public func f1() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1()</Name><USR>s:14swift_ide_test13EmptyCommentsC2f1yyF</USR><Declaration>public func f1()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]

  /** */
  public func f2() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f2()</Name><USR>s:14swift_ide_test13EmptyCommentsC2f2yyF</USR><Declaration>public func f2()</Declaration></Function>]

  /**
   */
  public func f3() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f3()</Name><USR>s:14swift_ide_test13EmptyCommentsC2f3yyF</USR><Declaration>public func f3()</Declaration></Function>]

  /**
   * Aaa.
   */
  public func f4() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f4()</Name><USR>s:14swift_ide_test13EmptyCommentsC2f4yyF</USR><Declaration>public func f4()</Declaration><Abstract><Para>Aaa.</Para></Abstract></Function>]
}

@objc public class HasThrowingFunction {
// CHECK: {{.*}}DocCommentAsXML=none

  /// Might throw something.
  ///
  /// - parameter x: A number
  /// - throws: An error if `x == 0`
  @objc public func f1(_ x: Int) /*throws*/ {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1(_:)</Name><USR>s:14swift_ide_test19HasThrowingFunctionC2f1ySiF</USR><Declaration>@objc public func f1(_ x: Int)</Declaration><Abstract><Para>Might throw something.</Para></Abstract><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters><ThrowsDiscussion><Para>An error if <codeVoice>x == 0</codeVoice></Para></ThrowsDiscussion></Function>]
}

@objc public class HorizontalRules {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Briefly.
  ///
  /// ------------------------------------
  ///
  /// The end.
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test15HorizontalRulesC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>Briefly.</Para></Abstract><Discussion><rawHTML><![CDATA[<hr/>]]></rawHTML><Para>The end.</Para></Discussion></Function>]
}

@objc public class ImplicitNameLink {
// CHECK: {{.*}}DocCommentAsXML=none
  /// [Apple][]
  ///
  /// [Apple]: https://www.apple.com/
  public func f0() {}
}

@objc public class IndentedBlockComment {
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
  public func f1() {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1()</Name><USR>s:14swift_ide_test20IndentedBlockCommentC2f1yyF</USR><Declaration>public func f1()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>First paragraph line. Second paragraph line.</Para><Para>Now for a code sample:</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[var x = 1]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// var y = 2]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[var z = 3]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing></Discussion></Function>]
  /**
                        Hugely indented brief.

                        First paragraph line.
                        Second paragraph line.

                        Now for a code sample:

                            var x = 1
                            // var y = 2
                            var z = 3
  */
  public func f2() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f2()</Name><USR>s:14swift_ide_test20IndentedBlockCommentC2f2yyF</USR><Declaration>public func f2()</Declaration><Abstract><Para>Hugely indented brief.</Para></Abstract><Discussion><Para>First paragraph line. Second paragraph line.</Para><Para>Now for a code sample:</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[var x = 1]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// var y = 2]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[var z = 3]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing></Discussion></Function>]
}

@objc public class InlineCode {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa `bbb` ccc.
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test10InlineCodeC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>Aaa <codeVoice>bbb</codeVoice> ccc.</Para></Abstract></Function>]
}

@objc public class InlineLink {
// CHECK: {{.*}}DocCommentAsXML=none
/// Aaa [bbb](/path/to/something) ccc.
public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test10InlineLinkC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>Aaa <Link href="/path/to/something">bbb</Link> ccc.</Para></Abstract></Function>]
}

@objc public class MultiLineBrief {
// CHECK: {{.*}}DocCommentAsXML=none

  /// Brief first line.
  /// Brief after softbreak.
  ///
  /// Some paragraph text.
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test14MultiLineBriefC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>Brief first line. Brief after softbreak.</Para></Abstract><Discussion><Para>Some paragraph text.</Para></Discussion></Function>]
}

@objc public class OrderedList {
// CHECK: {{.*}}DocCommentAsXML=none
/// 1. Aaa.
///
/// 2. Bbb.
///    Ccc.
public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test11OrderedListC2f0yyF</USR><Declaration>public func f0()</Declaration><Discussion><List-Number><Item><Para>Aaa.</Para></Item><Item><Para>Bbb. Ccc.</Para></Item></List-Number></Discussion></Function>]
}

/// - parameter x: A number
@objc public class ParamAndReturns {
// CHECK: {{.*}}DocCommentAsXML=[<Class file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>ParamAndReturns</Name><USR>s:14swift_ide_test15ParamAndReturnsC</USR><Declaration>@objc public class ParamAndReturns</Declaration><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters></Class>]
/// Aaa.  f0.
///
/// - parameter first: Bbb.
///
/// - parameter second: Ccc.  Ddd.
///   Eee.
public func f0(_ first: Int, second: Double) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0(_:second:)</Name><USR>s:14swift_ide_test15ParamAndReturnsC2f0ySi_Sd6secondtF</USR><Declaration>public func f0(_ first: Int, second: Double)</Declaration><Abstract><Para>Aaa.  f0.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Bbb.</Para></Discussion></Parameter><Parameter><Name>second</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Ccc.  Ddd. Eee.</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  f1.
///
/// - parameter first: Bbb.
///
/// - returns: Ccc.
///   Ddd.
public func f1(_ first: Int) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f1(_:)</Name><USR>s:14swift_ide_test15ParamAndReturnsC2f1ySiF</USR><Declaration>public func f1(_ first: Int)</Declaration><Abstract><Para>Aaa.  f1.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Bbb.</Para></Discussion></Parameter></Parameters><ResultDiscussion><Para>Ccc. Ddd.</Para></ResultDiscussion></Function>]
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  f2.
///
/// - parameter first:
///
/// - parameter second: Aaa.
///
/// - parameter third:
///   Bbb.
public func f2(_ first: Int, second: Double, third: Float) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f2(_:second:third:)</Name><USR>s:14swift_ide_test15ParamAndReturnsC2f2ySi_Sd6secondSf5thirdtF</USR><Declaration>public func f2(_ first: Int, second: Double, third: Float)</Declaration><Abstract><Para>Aaa.  f2.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para></Para></Discussion></Parameter><Parameter><Name>second</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Aaa.</Para></Discussion></Parameter><Parameter><Name>third</Name><Direction isExplicit="0">in</Direction><Discussion><Para> Bbb.</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none

/// Aaa.  f3.
///
/// - parameter first: Bbb.
/// - parameter second: Ccc.
/// - parameter third: Ddd.
public func f3(_ first: Int, second: Double, third: Float) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f3(_:second:third:)</Name><USR>s:14swift_ide_test15ParamAndReturnsC2f3ySi_Sd6secondSf5thirdtF</USR><Declaration>public func f3(_ first: Int, second: Double, third: Float)</Declaration><Abstract><Para>Aaa.  f3.</Para></Abstract><Parameters><Parameter><Name>first</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Bbb.</Para></Discussion></Parameter><Parameter><Name>second</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Ccc.</Para></Discussion></Parameter><Parameter><Name>third</Name><Direction isExplicit="0">in</Direction><Discussion><Para>Ddd.</Para></Discussion></Parameter></Parameters></Function>]
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
public func f4() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f4()</Name><USR>s:14swift_ide_test15ParamAndReturnsC2f4yyF</USR><Declaration>public func f4()</Declaration><Abstract><Para>Aaa.  f4.</Para></Abstract><ResultDiscussion><Para>Eee. Fff.</Para></ResultDiscussion></Function>]
}

@objc public class ParameterOutline{
// CHECK: {{.*}}DocCommentAsXML=none
/// - Parameters:
///   - x: A number
///   - y: A number
///
/// - PARAMETERS:
///   - z: A number
public func f0(_ x: Int, y: Int, z: Int) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0(_:y:z:)</Name><USR>s:14swift_ide_test16ParameterOutlineC2f0ySi_Si1ySi1ztF</USR><Declaration>public func f0(_ x: Int, y: Int, z: Int)</Declaration><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter><Parameter><Name>y</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter><Parameter><Name>z</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
}

@objc public class ParameterOutlineMiddle {
// CHECK: {{.*}}DocCommentAsXML=none
/// - This line should remain.
/// - Parameters:
///   - x: A number
///   - y: A number
/// - This line should also remain.
/// - parameter z: A number
public func f0(_ x: Int, y: Int, z: Int) {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0(_:y:z:)</Name><USR>s:14swift_ide_test22ParameterOutlineMiddleC2f0ySi_Si1ySi1ztF</USR><Declaration>public func f0(_ x: Int, y: Int, z: Int)</Declaration><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter><Parameter><Name>y</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter><Parameter><Name>z</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters><Discussion><List-Bullet><Item><Para>This line should remain.</Para></Item><Item><Para>This line should also remain.</Para></Item></List-Bullet></Discussion></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
}

@objc public class ReferenceLink {
// CHECK: {{.*}}DocCommentAsXML=none
  /// This is [a reference link] [1].
  ///
  /// [1]: http://developer.apple.com/
}


@objc public class Returns {
// CHECK: {{.*}}DocCommentAsXML=none
  /// - returns: A number
  public func f0() -> Int {
    return 0
  }
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test7ReturnsC2f0SiyF</USR><Declaration>public func f0() -&gt; Int</Declaration><ResultDiscussion><Para>A number</Para></ResultDiscussion></Function>]
}

@objc public class SeparateParameters {
// CHECK: {{.*}}DocCommentAsXML=none
  /// - Parameter x: A number
  public func f0(_ x: Int, y: Int) {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0(_:y:)</Name><USR>s:14swift_ide_test18SeparateParametersC2f0ySi_Si1ytF</USR><Declaration>public func f0(_ x: Int, y: Int)</Declaration><Parameters><Parameter><Name>x</Name><Direction isExplicit="0">in</Direction><Discussion><Para>A number</Para></Discussion></Parameter></Parameters></Function>]
// CHECK: {{.*}}DocCommentAsXML=none
// CHECK: {{.*}}DocCommentAsXML=none
}

@objc public class SetextHeaders {
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
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test13SetextHeadersC2f0yyF</USR><Declaration>public func f0()</Declaration><Discussion><rawHTML><![CDATA[<h1>]]></rawHTML>LEVEL ONE<rawHTML><![CDATA[</h1>]]></rawHTML><rawHTML><![CDATA[<h2>]]></rawHTML>LEVEL TWO<rawHTML><![CDATA[</h2>]]></rawHTML><rawHTML><![CDATA[<h3>]]></rawHTML>LEVEL THREE<rawHTML><![CDATA[</h3>]]></rawHTML><rawHTML><![CDATA[<h4>]]></rawHTML>LEVEL FOUR<rawHTML><![CDATA[</h4>]]></rawHTML><rawHTML><![CDATA[<h5>]]></rawHTML>LEVEL FIVE<rawHTML><![CDATA[</h5>]]></rawHTML><rawHTML><![CDATA[<h5>]]></rawHTML>LEVEL SIX<rawHTML><![CDATA[</h5>]]></rawHTML></Discussion></Function>]
}

@objc public class StrongEmphasis {
// CHECK: {{.*}}DocCommentAsXML=none
  /// Aaa **bbb** ccc.
  /// Aaa __bbb__ ccc.
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test14StrongEmphasisC2f0yyF</USR><Declaration>public func f0()</Declaration><Abstract><Para>Aaa <bold>bbb</bold> ccc. Aaa <bold>bbb</bold> ccc.</Para></Abstract></Function>]
}

@objc public class UnorderedList {
// CHECK: {{.*}}DocCommentAsXML=none
  /// * Aaa.
  ///
  /// * Bbb.
  ///   Ccc.
  ///
  /// - Ddd.
  /// - Eee.
  ///   - Fff.
  public func f0() {}
// CHECK: {{.*}}DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>f0()</Name><USR>s:14swift_ide_test13UnorderedListC2f0yyF</USR><Declaration>public func f0()</Declaration><Discussion><List-Bullet><Item><Para>Aaa.</Para></Item><Item><Para>Bbb. Ccc.</Para></Item></List-Bullet><List-Bullet><Item><Para>Ddd.</Para></Item><Item><Para>Eee.</Para><List-Bullet><Item><Para>Fff.</Para></Item></List-Bullet></Item></List-Bullet></Discussion></Function>]
}

/// Brief.
///
/// ```
/// thisIsASwiftCodeExample()
/// ```
public func codeListingWithDefaultLanguage() {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}} line="{{.*}}" column="{{.*}}"><Name>codeListingWithDefaultLanguage()</Name><USR>s:14swift_ide_test30codeListingWithDefaultLanguageyyF</USR><Declaration>public func codeListingWithDefaultLanguage()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><CodeListing language="swift"><zCodeLineNumbered><![CDATA[thisIsASwiftCodeExample()]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing></Discussion></Function>] CommentXMLValid


/// Brief.
///
/// ```c++
/// Something::Something::create();
/// ```
public func codeListingWithOtherLanguage() {}
// CHECK: DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>codeListingWithOtherLanguage()</Name><USR>s:14swift_ide_test28codeListingWithOtherLanguageyyF</USR><Declaration>public func codeListingWithOtherLanguage()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><CodeListing language="c++"><zCodeLineNumbered><![CDATA[Something::Something::create();]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing></Discussion></Function>]
