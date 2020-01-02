// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_1 | %FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_1P | %FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_1P
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_2 | %FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_2P | %FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_2P

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_1 | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_1P | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_1P

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_2 | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_2P | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_2P

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_3P | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_3P

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_SELECTOR_1 | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_SELECTOR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_VAL_META_1 | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_VAL_META_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_VAL_META_2 | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_VAL_META_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1 | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_INIT_FROM_PROT_NODOT | %FileCheck %s -check-prefix=DEFAULT_INIT_FROM_PROT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_INIT_FROM_PROT_DOT | %FileCheck %s -check-prefix=DEFAULT_INIT_FROM_PROT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_INIT_FROM_PROT_PAREN | %FileCheck %s -check-prefix=DEFAULT_INIT_FROM_PROT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE1 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE2_1 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE2_NOINIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE2_2 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE2_NOINIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE2_3 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE2_NOINIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE2_4 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE2_SHOW_INIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE2_5 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE2_SHOW_INIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE2_6 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE2_NOINIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE3 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE4 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE5 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_FROM_METATYPE6 | %FileCheck %s -check-prefix=INIT_FROM_METATYPE6

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=HAVE_RPAREN_1 | %FileCheck %s -check-prefix=HAVE_RPAREN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=HAVE_RPAREN_2 | %FileCheck %s -check-prefix=HAVE_RPAREN_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=HAVE_COMMA_1 -code-complete-call-pattern-heuristics | %FileCheck %s -check-prefix=HAVE_COMMA_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=HAVE_COMMA_1 | %FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_1P
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=WITH_ALIAS_1 | %FileCheck %s -check-prefix=WITH_ALIAS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_IN_INIT_1 | %FileCheck %s -check-prefix=CLOSURE_IN_INIT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_IN_INIT_2 | %FileCheck %s -check-prefix=CLOSURE_IN_INIT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_IN_INIT_3 | %FileCheck %s -check-prefix=CLOSURE_IN_INIT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_IN_INIT_4 | %FileCheck %s -check-prefix=CLOSURE_IN_INIT_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABLE_1 | %FileCheck %s -check-prefix=AVAILABLE_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABLE_2 | %FileCheck %s -check-prefix=AVAILABLE_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEPENDENT_IN_CLOSURE_1 | %FileCheck %s -check-prefix=DEPENDENT_IN_CLOSURE_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEPENDENT_IN_CLOSURE_2 | %FileCheck %s -check-prefix=DEPENDENT_IN_CLOSURE_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_WITH_UNRESOLVEDTYPE_1 | %FileCheck %s -check-prefix=INIT_WITH_UNRESOLVEDTYPE_1

func freeFunc() {}

//===---
//===--- Test that we complete calls to constructors (except for 'super'-based calls, which are tested separately).
//===---

struct ImplicitConstructors1 {
}

func testImplicitConstructors1() {
  ImplicitConstructors1#^IMPLICIT_CONSTRUCTORS_1^#
// IMPLICIT_CONSTRUCTORS_1: Begin completions, 3 items
// IMPLICIT_CONSTRUCTORS_1-DAG: Decl[Constructor]/CurrNominal: ()[#ImplicitConstructors1#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_1-DAG: Keyword[self]/CurrNominal:     .self[#ImplicitConstructors1.Type#]; name=self
// IMPLICIT_CONSTRUCTORS_1-DAG: Keyword/CurrNominal:           .Type[#ImplicitConstructors1.Type#]; name=Type
// IMPLICIT_CONSTRUCTORS_1: End completions
}
func testImplicitConstructors1P() {
  ImplicitConstructors1(#^IMPLICIT_CONSTRUCTORS_1P^#
// IMPLICIT_CONSTRUCTORS_1P: Begin completions, 1 items
// IMPLICIT_CONSTRUCTORS_1P: Decl[Constructor]/CurrNominal: ['('][')'][#ImplicitConstructors1#]; name=
// IMPLICIT_CONSTRUCTORS_1P: End completions
}

struct ImplicitConstructors2 {
  var instanceVar = 0
}

func testImplicitConstructors2() {
  ImplicitConstructors2#^IMPLICIT_CONSTRUCTORS_2^#
// IMPLICIT_CONSTRUCTORS_2: Begin completions, 5 items
// IMPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ()[#ImplicitConstructors2#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ({#instanceVar: Int#})[#ImplicitConstructors2#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ()[#ImplicitConstructors2#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_2-DAG: Keyword[self]/CurrNominal:     .self[#ImplicitConstructors2.Type#]; name=self
// IMPLICIT_CONSTRUCTORS_2-DAG: Keyword/CurrNominal:           .Type[#ImplicitConstructors2.Type#]; name=Type
// IMPLICIT_CONSTRUCTORS_2: End completions
}
func testImplicitConstructors2P() {
  ImplicitConstructors2(#^IMPLICIT_CONSTRUCTORS_2P^#
// IMPLICIT_CONSTRUCTORS_2P: Begin completions, 3 items
// IMPLICIT_CONSTRUCTORS_2P-DAG: Decl[Constructor]/CurrNominal: ['('][')'][#ImplicitConstructors2#]; name=
// IMPLICIT_CONSTRUCTORS_2P-DAG: Decl[Constructor]/CurrNominal: ['(']{#instanceVar: Int#}[')'][#ImplicitConstructors2#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_2P-DAG: Decl[Constructor]/CurrNominal: ['('][')'][#ImplicitConstructors2#]; name=
// IMPLICIT_CONSTRUCTORS_2P: End completions
}

struct ExplicitConstructors1 {
  init() {}
  init(a: Int) {}
  init(a: Int, b: Float) {}
}

func testExplicitConstructors1() {
  ExplicitConstructors1#^EXPLICIT_CONSTRUCTORS_1^#
// EXPLICIT_CONSTRUCTORS_1: Begin completions, 5 items
// EXPLICIT_CONSTRUCTORS_1-DAG: Decl[Constructor]/CurrNominal: ()[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1-DAG: Decl[Constructor]/CurrNominal: ({#a: Int#})[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1-DAG: Decl[Constructor]/CurrNominal: ({#a: Int#}, {#b: Float#})[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1-DAG: Keyword[self]/CurrNominal: .self[#ExplicitConstructors1.Type#]; name=self
// EXPLICIT_CONSTRUCTORS_1-DAG: Keyword/CurrNominal:       .Type[#ExplicitConstructors1.Type#]; name=Type
// EXPLICIT_CONSTRUCTORS_1: End completions
}
func testExplicitConstructors1P() {
  ExplicitConstructors1(#^EXPLICIT_CONSTRUCTORS_1P^#
// EXPLICIT_CONSTRUCTORS_1P: Begin completions
// EXPLICIT_CONSTRUCTORS_1P-NEXT: Decl[Constructor]/CurrNominal: ['('][')'][#ExplicitConstructors1#]; name=
// EXPLICIT_CONSTRUCTORS_1P-NEXT: Decl[Constructor]/CurrNominal: ['(']{#a: Int#}[')'][#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1P-NEXT: Decl[Constructor]/CurrNominal: ['(']{#a: Int#}, {#b: Float#}[')'][#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1P-NEXT: End completions
}

ExplicitConstructors1#^EXPLICIT_CONSTRUCTORS_2^#

// EXPLICIT_CONSTRUCTORS_2: Begin completions, 5 items
// EXPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ()[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ({#a: Int#})[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ({#a: Int#}, {#b: Float#})[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2-DAG: Keyword[self]/CurrNominal:     .self[#ExplicitConstructors1.Type#]; name=self
// EXPLICIT_CONSTRUCTORS_2-DAG: Keyword/CurrNominal:           .Type[#ExplicitConstructors1.Type#]; name=Type
// EXPLICIT_CONSTRUCTORS_2: End completions

ExplicitConstructors1(#^EXPLICIT_CONSTRUCTORS_2P^#

// EXPLICIT_CONSTRUCTORS_2P: Begin completions
// EXPLICIT_CONSTRUCTORS_2P-DAG: Decl[Constructor]/CurrNominal: ['(']{#a: Int#}[')'][#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2P-DAG: Decl[Constructor]/CurrNominal: ['(']{#a: Int#}, {#b: Float#}[')'][#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2P: End completions


struct ExplicitConstructors3 {
  init() {}
  init(_ a: Int) {}
  init(a: Int, b: Float) {}
}

func testExplicitConstructors3P() {
  ExplicitConstructors3(#^EXPLICIT_CONSTRUCTORS_3P^#
// EXPLICIT_CONSTRUCTORS_3P: Begin completions
// EXPLICIT_CONSTRUCTORS_3P-DAG: Decl[Constructor]/CurrNominal: ['(']{#(a): Int#}[')'][#ExplicitConstructors3#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_3P-DAG: Decl[Constructor]/CurrNominal: ['(']{#a: Int#}, {#b: Float#}[')'][#ExplicitConstructors3#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_3P: End completions
}


struct ExplicitConstructorsSelector1 {
  init(int a : Int) {}
  init(int a : Int, andFloat b : Float) {}
}

func testExplicitConstructorsSelector1() {
  ExplicitConstructorsSelector1#^EXPLICIT_CONSTRUCTORS_SELECTOR_1^#
// EXPLICIT_CONSTRUCTORS_SELECTOR_1: Begin completions, 4 items
// EXPLICIT_CONSTRUCTORS_SELECTOR_1-DAG: Decl[Constructor]/CurrNominal: ({#int: Int#})[#ExplicitConstructorsSelector1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_1-DAG: Decl[Constructor]/CurrNominal: ({#int: Int#}, {#andFloat: Float#})[#ExplicitConstructorsSelector1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_1-DAG: Keyword[self]/CurrNominal:     .self[#ExplicitConstructorsSelector1.Type#]; name=self
// EXPLICIT_CONSTRUCTORS_SELECTOR_1-DAG: Keyword/CurrNominal:           .Type[#ExplicitConstructorsSelector1.Type#]; name=Type
// EXPLICIT_CONSTRUCTORS_SELECTOR_1: End completions
}

func testExplicitConstructorsValueMetatype() {
  var SS = ExplicitConstructorsSelector1.self
  SS#^EXPLICIT_CONSTRUCTORS_VAL_META_1^#
  SS(#^EXPLICIT_CONSTRUCTORS_VAL_META_2^#

// EXPLICIT_CONSTRUCTORS_VAL_META_1: Decl[Constructor]/CurrNominal: .init({#int: Int#})[#ExplicitConstructorsSelector1#]
// EXPLICIT_CONSTRUCTORS_VAL_META_1: Decl[Constructor]/CurrNominal: .init({#int: Int#}, {#andFloat: Float#})[#ExplicitConstructorsSelector1#]

// EXPLICIT_CONSTRUCTORS_VAL_META_2-NOT: Decl[Constructor]
}

struct ExplicitConstructorsSelector2 {
  init(noArgs _ : ()) {}
  init(_ a : Int) {}
  init(_ a : Int, withFloat b : Float) {}
  init(int a : Int, _ b : Float) {}
}

func testExplicitConstructorsSelector2() {
  ExplicitConstructorsSelector2#^EXPLICIT_CONSTRUCTORS_SELECTOR_2^#
// EXPLICIT_CONSTRUCTORS_SELECTOR_2: Begin completions, 6 items
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Decl[Constructor]/CurrNominal: ({#noArgs: ()#})[#ExplicitConstructorsSelector2#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Decl[Constructor]/CurrNominal: ({#Int#})[#ExplicitConstructorsSelector2#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Decl[Constructor]/CurrNominal: ({#Int#}, {#withFloat: Float#})[#ExplicitConstructorsSelector2#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Decl[Constructor]/CurrNominal: ({#int: Int#}, {#Float#})[#ExplicitConstructorsSelector2#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Keyword[self]/CurrNominal:     .self[#ExplicitConstructorsSelector2.Type#]; name=self
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Keyword/CurrNominal:           .Type[#ExplicitConstructorsSelector2.Type#]; name=Type
// EXPLICIT_CONSTRUCTORS_SELECTOR_2: End completions
}

class ExplicitConstructorsBase1 {
  init() {}
  init(a : Int) {}
}

class ExplicitConstructorsDerived1 : ExplicitConstructorsBase1 {
  init() {}
  init(a : Int) {}
}

class ExplicitConstructorsDerived2 : ExplicitConstructorsBase1 {
  init() {}
  required init(a : Int) {}
  class func foo() {
    self.#^INIT_FROM_METATYPE5^#
  }
}

func testExplicitConstructorsBaseDerived1() {
  ExplicitConstructorsDerived1#^EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1^#
}
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1: Begin completions, 4 items
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1-DAG: Decl[Constructor]/CurrNominal:  ()[#ExplicitConstructorsDerived1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1-DAG: Decl[Constructor]/CurrNominal:  ({#a: Int#})[#ExplicitConstructorsDerived1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1-DAG: Keyword[self]/CurrNominal:     .self[#ExplicitConstructorsDerived1.Type#]; name=self
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1-DAG: Keyword/CurrNominal:           .Type[#ExplicitConstructorsDerived1.Type#]; name=Type
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1: End completions

func testGetInitFromMetatype1() {
  ExplicitConstructorsBase1.#^INIT_FROM_METATYPE1^#
}

// INIT_FROM_METATYPE1: Begin completions
// INIT_FROM_METATYPE1-NEXT: Keyword[self]/CurrNominal:          self[#ExplicitConstructorsBase1.Type#]; name=self
// INIT_FROM_METATYPE1-NEXT: Keyword/CurrNominal:                Type[#ExplicitConstructorsBase1.Type#]; name=Type
// INIT_FROM_METATYPE1-NEXT: Decl[Constructor]/CurrNominal:      init()[#ExplicitConstructorsBase1#]{{; name=.+$}}
// INIT_FROM_METATYPE1-NEXT: Decl[Constructor]/CurrNominal:      init({#a: Int#})[#ExplicitConstructorsBase1#]{{; name=.+$}}
// INIT_FROM_METATYPE1-NEXT: End completions

func testGetInitFromMetatype2() {
  var S1 = ExplicitConstructorsBase1.self
  var S2 = ExplicitConstructorsDerived2.self
  S1.#^INIT_FROM_METATYPE2_1^#
  S1#^INIT_FROM_METATYPE2_2^#
  S1(#^INIT_FROM_METATYPE2_3^#
  S2.#^INIT_FROM_METATYPE2_4^#
  S2#^INIT_FROM_METATYPE2_5^#
  S2(#^INIT_FROM_METATYPE2_6^#
}
// INIT_FROM_METATYPE2_NOINIT-NOT: Decl[Constructor]

// INIT_FROM_METATYPE2_SHOW_INIT-NOT: Decl[Constructor]
// INIT_FROM_METATYPE2_SHOW_INIT: Decl[Constructor]/CurrNominal: {{init|.init}}({#a: Int#})[#ExplicitConstructorsDerived2#]
// INIT_FROM_METATYPE2_SHOW_INIT-NOT: Decl[Constructor]

func testGetInitFromMetatype3() {
  var SS = ExplicitConstructorsBase1.self
  type(of: SS).#^INIT_FROM_METATYPE3^#
}

// INIT_FROM_METATYPE3-NOT: Decl[Constructor]/CurrNominal:      init()[#ExplicitConstructorsBase1#]{{; name=.+$}}

func testGetInitFromMetatype4() {
  var a = ExplicitConstructorsDerived2()
  type(of: a).#^INIT_FROM_METATYPE4^#
}

// INIT_FROM_METATYPE4: Decl[Constructor]/CurrNominal: init({#a: Int#})[#ExplicitConstructorsDerived2#]; name=init(a: Int)
// INIT_FROM_METATYPE4-NOT: Decl[Constructor]/CurrNominal:      init()[#ExplicitConstructorsDerived2#]{{; name=.+$}}

struct ExplicitConstructorsDerived3 {
  init() {}
  required init(a : Int) {}
  static func foo() {
    self.#^INIT_FROM_METATYPE6^#
  }
// INIT_FROM_METATYPE6: Decl[Constructor]/CurrNominal:      init()[#ExplicitConstructorsDerived3#]{{; name=.+$}}
// INIT_FROM_METATYPE6: Decl[Constructor]/CurrNominal:      init({#a: Int#})[#ExplicitConstructorsDerived3#]{{; name=.+$}}
}

func testHaveRParen1() {
  ImplicitConstructors1(#^HAVE_RPAREN_1^#)
// HAVE_RPAREN_1: Begin completions, 1 items
// HAVE_RPAREN_1: Decl[Constructor]/CurrNominal: ['('][')'][#ImplicitConstructors1#]; name=
// HAVE_RPAREN_1: End completions
}

func testHaveRParen2() {
  ImplicitConstructors2(#^HAVE_RPAREN_2^#)
// HAVE_RPAREN_2: Begin completions, 3 items
// HAVE_RPAREN_2-DAG: Decl[Constructor]/CurrNominal: ['('][')'][#ImplicitConstructors2#]; name=
// HAVE_RPAREN_2-DAG: Decl[Constructor]/CurrNominal: ['(']{#instanceVar: Int#}[')'][#ImplicitConstructors2#]{{; name=.+$}}
// HAVE_RPAREN_2-DAG: Decl[Constructor]/CurrNominal: ['('][')'][#ImplicitConstructors2#]; name=
// HAVE_RPAREN_2: End completions
}

func testHaveComma1() {
  ExplicitConstructors1(#^HAVE_COMMA_1^#,
// HAVE_COMMA_1-NOT: Decl[Constructor]
}

//===--- Test that we show default constuctors inherited from protocols

protocol ProtDefaultInit {}
extension ProtDefaultInit { init(foo: Int) {}}

class ConformsToProtDefaultInit: ProtDefaultInit {
  init(bar: Int) {}
}

func testHasDefaultInitFromProtocol() {
  ConformsToProtDefaultInit#^DEFAULT_INIT_FROM_PROT_NODOT^#
  ConformsToProtDefaultInit.#^DEFAULT_INIT_FROM_PROT_DOT^#
  ConformsToProtDefaultInit(#^DEFAULT_INIT_FROM_PROT_PAREN^#

// DEFAULT_INIT_FROM_PROT-DAG: Decl[Constructor]/CurrNominal:  {{.+}}{#bar: Int#}
// DEFAULT_INIT_FROM_PROT-DAG: Decl[Constructor]/Super:        {{.+}}{#foo: Int#}
}

class WithAlias1 {
  init(busted: B) {}
  init(working: Int) {}
}
typealias Alias1 = WithAlias1
func testWithAlias1() {
  Alias1#^WITH_ALIAS_1^#
}
// WITH_ALIAS_1: Decl[Constructor]/CurrNominal:      ({#working: Int#})[#Alias1#];

struct ClosureInInit1 {
  struct S {init(_: Int) {}}
  var prop1: S = {
    return S(#^CLOSURE_IN_INIT_1^#
  }
// CLOSURE_IN_INIT_1: Decl[Constructor]/CurrNominal{{(/TypeRelation\[Identical\])?}}:      ['(']{#Int#}[')'][#S#];
  var prop2: S = {
    return S(#^CLOSURE_IN_INIT_2^#
  }()
  var prop3: S = {
    S(#^CLOSURE_IN_INIT_3^#
  }
  var prop3: S = {
    S(#^CLOSURE_IN_INIT_4^#
  }()
}

public class AvailableTest {

  @available(swift, obsoleted: 4)
  init(opt: Int) { }

  @available(swift, introduced: 4)
  init?(opt: Int) { }

  init(normal1: Int) { }
  init(normal2: Int) { }


}
func testAvailable() {
  let _ = AvailableTest(#^AVAILABLE_1^#
// AVAILABLE_1: Begin completions, 3 items
// AVAILABLE_1-DAG: Decl[Constructor]/CurrNominal:      ['(']{#opt: Int#}[')'][#AvailableTest?#]; name=opt: Int
// AVAILABLE_1-DAG: Decl[Constructor]/CurrNominal:      ['(']{#normal1: Int#}[')'][#AvailableTest#]; name=normal1: Int
// AVAILABLE_1-DAG: Decl[Constructor]/CurrNominal:      ['(']{#normal2: Int#}[')'][#AvailableTest#]; name=normal2: Int
// AVAILABLE_1: End completions

  let _ = AvailableTest.init(#^AVAILABLE_2^#
}

protocol DataType {
  associatedtype Content
}
class DependentTypeInClosure<Data: DataType> {
  init(_ arg: Data, fn: (Data.Content) -> Void) {}
  init(arg: Data, fn: () -> Data.Content) {}
}
func testDependentTypeInClosure() {
  let _: DependentTypeInClosure = .#^DEPENDENT_IN_CLOSURE_3^#
  let _ = DependentTypeInClosure(#^DEPENDENT_IN_CLOSURE_1^#)
// DEPENDENT_IN_CLOSURE_1: Begin completions
// DEPENDENT_IN_CLOSURE_1-DAG: Decl[Constructor]/CurrNominal:      ['(']{#(arg): _#}, {#fn: (_.Content) -> Void##(_.Content) -> Void#}[')'][#DependentTypeInClosure<_>#];
// DEPENDENT_IN_CLOSURE_1-DAG: Decl[Constructor]/CurrNominal:      ['(']{#arg: _#}, {#fn: () -> _.Content##() -> _.Content#}[')'][#DependentTypeInClosure<_>#];
// DEPENDENT_IN_CLOSURE_1: End completions

  let _ = DependentTypeInClosure.#^DEPENDENT_IN_CLOSURE_2^#
// DEPENDENT_IN_CLOSURE_2: Begin completions, 4 items
// DEPENDENT_IN_CLOSURE_2-DAG: Keyword[self]/CurrNominal:          self[#DependentTypeInClosure<_>.Type#]; name=self
// DEPENDENT_IN_CLOSURE_2-DAG: Keyword/CurrNominal:                Type[#DependentTypeInClosure<_>.Type#]; name=Type
// DEPENDENT_IN_CLOSURE_2-DAG: Decl[Constructor]/CurrNominal:      init({#(arg): _#}, {#fn: (_.Content) -> Void##(_.Content) -> Void#})[#DependentTypeInClosure<_>#]; name=init(arg: _, fn: (_.Content) -> Void)
// DEPENDENT_IN_CLOSURE_2-DAG: Decl[Constructor]/CurrNominal:      init({#arg: _#}, {#fn: () -> _.Content##() -> _.Content#})[#DependentTypeInClosure<_>#]; name=init(arg: _, fn: () -> _.Content)
// DEPENDENT_IN_CLOSURE_2: End completions
}
struct InitWithUnresolved<Data: DataType> where Data.Content: Comparable {
  init(arg: Data, fn: (Data.Content) -> Void) {}
}
extension InitWithUnresolved where Self.Data: Comparable {
  init(arg2: Data) {}
}
func testInitWithUnresolved() {
  let _ = InitWithUnresolved(#^INIT_WITH_UNRESOLVEDTYPE_1^#
// INIT_WITH_UNRESOLVEDTYPE_1: Begin completions, 2 items
// INIT_WITH_UNRESOLVEDTYPE_1-DAG: Decl[Constructor]/CurrNominal:      ['(']{#arg: _#}, {#fn: (_.Content) -> Void##(_.Content) -> Void#}[')'][#InitWithUnresolved<_>#];
// INIT_WITH_UNRESOLVEDTYPE_1-DAG: Decl[Constructor]/CurrNominal:      ['(']{#arg2: _#}[')'][#InitWithUnresolved<_>#];
// INIT_WITH_UNRESOLVEDTYPE_1: End completions
}
