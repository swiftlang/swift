// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG1 | %FileCheck %s -check-prefix=EXPECT_OINT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG2 | %FileCheck %s -check-prefix=ARG-NAME1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG3 | %FileCheck %s -check-prefix=ARG-NAME2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG4 | %FileCheck %s -check-prefix=EXPECT_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG5 | %FileCheck %s -check-prefix=EXPECT_OSTRING
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG6 | %FileCheck %s -check-prefix=ARG-NAME2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG7 | %FileCheck %s -check-prefix=ARG-NAME1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG8 | %FileCheck %s -check-prefix=EXPECT_STRING

// RUN-FIXME: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD1 | %FileCheck %s -check-prefix=OVERLOAD1
// RUN-FIXME: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD2 | %FileCheck %s -check-prefix=OVERLOAD2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD3 | %FileCheck %s -check-prefix=OVERLOAD3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD4 | %FileCheck %s -check-prefix=OVERLOAD4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER1 | %FileCheck %s -check-prefix=MEMBER1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER2 | %FileCheck %s -check-prefix=MEMBER2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER3 | %FileCheck %s -check-prefix=MEMBER3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER4 | %FileCheck %s -check-prefix=MEMBER4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER5 | %FileCheck %s -check-prefix=MEMBER2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER6 | %FileCheck %s -check-prefix=MEMBER4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER7 | %FileCheck %s -check-prefix=MEMBER7
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER8 | %FileCheck %s -check-prefix=MEMBER8
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER9 | %FileCheck %s -check-prefix=MEMBER1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FARG1 | %FileCheck %s -check-prefix=EXPECT_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FARG2 | %FileCheck %s -check-prefix=EXPECT_STRING
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FARG3 | %FileCheck %s -check-prefix=MEMBER2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FARG4 | %FileCheck %s -check-prefix=MEMBER4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FARG5 | %FileCheck %s -check-prefix=MEMBER2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FARG6 | %FileCheck %s -check-prefix=FARG6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FARG7 | %FileCheck %s -check-prefix=EXPECT_OINT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIRST_ARG_NAME_1 | %FileCheck %s -check-prefix=FIRST_ARG_NAME_PATTERN
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIRST_ARG_NAME_2 | %FileCheck %s -check-prefix=FIRST_ARG_NAME_PATTERN
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIRST_ARG_NAME_3 -code-complete-call-pattern-heuristics | %FileCheck %s -check-prefix=FIRST_ARG_NAME_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIRST_ARG_NAME_3 | %FileCheck %s -check-prefix=FIRST_ARG_NAME_4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BOUND_GENERIC_1_1 | %FileCheck %s -check-prefix=BOUND_GENERIC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BOUND_GENERIC_1_2 | %FileCheck %s -check-prefix=BOUND_GENERIC_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EMPTY_OVERLOAD_1 | %FileCheck %s -check-prefix=EMPTY_OVERLOAD
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EMPTY_OVERLOAD_2 | %FileCheck %s -check-prefix=EMPTY_OVERLOAD

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CALLARG_IUO | %FileCheck %s -check-prefix=CALLARG_IUO
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BOUND_IUO | %FileCheck %s -check-prefix=MEMBEROF_IUO
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FORCED_IUO | %FileCheck %s -check-prefix=MEMBEROF_IUO

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_TO_GENERIC | %FileCheck %s -check-prefix=GENERIC_TO_GENERIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_CLOSURE | %FileCheck %s -check-prefix=NESTED_CLOSURE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SHUFFLE_1 | %FileCheck %s -check-prefix=SHUFFLE_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SHUFFLE_2 | %FileCheck %s -check-prefix=SHUFFLE_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SHUFFLE_3 | %FileCheck %s -check-prefix=SHUFFLE_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_1 | %FileCheck %s -check-prefix=SUBSCRIPT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_1_DOT | %FileCheck %s -check-prefix=SUBSCRIPT_1_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_2 | %FileCheck %s -check-prefix=SUBSCRIPT_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_2_DOT | %FileCheck %s -check-prefix=SUBSCRIPT_2_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_3 | %FileCheck %s -check-prefix=SUBSCRIPT_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_3_DOT | %FileCheck %s -check-prefix=SUBSCRIPT_3_DOT

var i1 = 1
var i2 = 2
var oi1 : Int?
var oi2 : Int?
var s1 = ""
var s2 = ""
var os1 : String?
var os2 : String?
func voidGen() {}
func ointGen() -> Int? { return 1 }
func intGen() -> Int {return 1}
func ostringGen() -> String? {return ""}
func stringGen() -> String {return ""}
func foo(_ a : Int) {}
func foo(_ a : Int, b1 :Int?) {}
func foo(_ a : Int, b2 :Int?, b3: Int?) {}
func foo1(_ a : Int, b : Int) {}
func bar(_ a : String, b : String?) {}
func bar1(_ a : String, b1 : String) {}
func bar1(_ a : String, b2 : String) {}
func foo3(_ a: Int?) {}

class InternalGen {
  func InternalIntGen() -> Int { return 0 }
  func InternalIntOpGen() -> Int? {return 0 }
  func InternalStringGen() -> String { return "" }
  func InternalStringOpGen() -> String? {return ""}
  func InternalIntTaker(_ i1 : Int, i2 : Int) {}
  func InternalStringTaker(_ s1: String, s2 : String) {}
}

class Gen {
  var IG = InternalGen()
  func IntGen() -> Int { return 0 }
  func IntOpGen() -> Int? {return 0 }
  func StringGen() -> String { return "" }
  func StringOpGen() -> String? {return ""}
  func IntTaker(_ i1 : Int, i2 : Int) {}
  func StringTaker(_ s1: String, s2 : String) {}
}

func GenGenerator(_ i : Int) -> Gen { return Gen() }

enum SimpleEnum {
  case foo, bar, baz
}

class C1 {
  func f1() {
    foo(3, b: #^ARG1^#)
  }
  func f2() {
    foo(3, #^ARG2^#)
  }
  func f3() {
    foo1(2, #^ARG3^#
  }
  func f4() {
    foo1(2, b : #^ARG4^#
  }

  func f5() {
    foo(#^FARG1^#, b1 : 2)
  }

  func f6() {
    bar(#^FARG2^#
  }

  func f7() {
    foo3(#^FARG7^#)
  }
}

// ARG-NAME1: Begin completions, 2 items
// ARG-NAME1-DAG: Keyword/ExprSpecific:               b1: [#Argument name#]; name=b1:
// ARG-NAME1-DAG: Keyword/ExprSpecific:               b2: [#Argument name#]; name=b2:

// ARG-NAME2: Begin completions, 1 items
// ARG-NAME2-DAG: Keyword/ExprSpecific:               b: [#Argument name#]; name=b:

// EXPECT_OINT: Begin completions
// EXPECT_OINT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_OINT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: i2[#Int#]; name=i2
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: i1[#Int#]; name=i1
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: oi2[#Int?#]; name=oi2
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: oi1[#Int?#]; name=oi1
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule:         os1[#String?#]; name=os1
// EXPECT_OINT: End completions

// EXPECT_INT: Begin completions
// EXPECT_INT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_INT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_INT-DAG: Decl[FreeFunction]/CurrModule/NotRecommended/TypeRelation[Invalid]: voidGen()[#Void#]; name=voidGen()
// EXPECT_INT-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: intGen()[#Int#]; name=intGen()
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: i1[#Int#]; name=i1
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: i2[#Int#]; name=i2
// EXPECT_INT-DAT: Decl[Struct]/OtherModule[Swift]/TypeRelation[Identical]: Int[#Int#]
// EXPECT_INT-DAG: Decl[FreeFunction]/CurrModule:      ointGen()[#Int?#]; name=ointGen()
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule:         oi1[#Int?#]; name=oi1
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule:         os2[#String?#]; name=os2
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule:         oi2[#Int?#]; name=oi2
// EXPECT_INT: End completions

class C2 {
  func f1() {
    bar("", b: #^ARG5^#)
  }
  func f2() {
    bar("", #^ARG6^#)
  }
  func f3() {
    bar1("", #^ARG7^#
  }
  func f4() {
    bar1("", b : #^ARG8^#
  }
}

// EXPECT_OSTRING: Begin completions
// EXPECT_OSTRING-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_OSTRING-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_OSTRING-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Convertible]: stringGen()[#String#]; name=stringGen()
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: s2[#String#]; name=s2
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: s1[#String#]; name=s1
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: os1[#String?#]; name=os1
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: os2[#String?#]; name=os2
// EXPECT_OSTRING-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: ostringGen()[#String?#]; name=ostringGen()
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule:         i1[#Int#]; name=i1
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule:         i2[#Int#]; name=i2
// EXPECT_OSTRING: End completions

// EXPECT_STRING: Begin completions
// EXPECT_STRING-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_STRING-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_STRING-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: stringGen()[#String#]; name=stringGen()
// EXPECT_STRING-DAG: Decl[Struct]/OtherModule[Swift]/TypeRelation[Identical]: String[#String#]
// EXPECT_STRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: s1[#String#]; name=s1
// EXPECT_STRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: s2[#String#]; name=s2
// EXPECT_STRING-DAG: Decl[GlobalVar]/CurrModule:         os1[#String?#]; name=os1
// EXPECT_STRING-DAG: Decl[GlobalVar]/CurrModule:         os2[#String?#]; name=os2
// EXPECT_STRING: End completions

func foo2(_ a : C1, b1 : C2) {}
func foo2(_ a : C2, b2 : C1) {}

class C3 {
  var C1I = C1()
  var C2I = C2()
  func f1() {
    foo2(C1I, #^OVERLOAD1^#)
  }
  func f2() {
    foo2(C2I, #^OVERLOAD2^#)
  }
  func f2() {
    foo2(C1I, b1: #^OVERLOAD3^#)
  }
  func f2() {
    foo2(C2I, b2: #^OVERLOAD4^#)
  }
}

// OVERLOAD1: Begin completions, 1 items
// OVERLOAD1-NEXT: Keyword/ExprSpecific:               b1: [#Argument name#]; name=b1:
// OVERLOAD1-NEXT: End completions

// OVERLOAD2: Begin completions, 1 items
// OVERLOAD2-NEXT: Keyword/ExprSpecific:               b2: [#Argument name#]; name=b2:
// OVERLOAD2-NEXT: End completions

// OVERLOAD3: Begin completions
// OVERLOAD3-DAG: Decl[InstanceVar]/CurrNominal:      C1I[#C1#]; name=C1I
// OVERLOAD3-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// OVERLOAD3-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]: C2I[#C2#]; name=C2I
// OVERLOAD3-DAG: Decl[Class]/CurrModule/TypeRelation[Identical]: C2[#C2#]
// OVERLOAD3: End completions

// FIXME: This should be a negative test case
// NEGATIVE_OVERLOAD_3-NOT: Decl[Class]{{.*}} C1

// OVERLOAD4: Begin completions
// OVERLOAD4-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]: C1I[#C1#]; name=C1I
// OVERLOAD4-DAG: Decl[InstanceVar]/CurrNominal:      C2I[#C2#]; name=C2I
// OVERLOAD4-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// OVERLOAD4-DAG: Decl[Class]/CurrModule/TypeRelation[Identical]: C1[#C1#]
// OVERLOAD4: End completions

// FIXME: This should be a negative test case
// NEGATIVE_OVERLOAD4-NOT: Decl[Class]{{.*}} C2

class C4 {
  func f1(_ G : Gen) {
    foo(1, b1: G.#^MEMBER1^#
  }

  func f2(_ G : Gen) {
    foo1(2, b : G.#^MEMBER2^#
  }

  func f3(_ G : Gen) {
    bar("", b1 : G.#^MEMBER3^#
  }

  func f4(_ G : Gen) {
    bar1("", b1 : G.#^MEMBER4^#
  }

  func f5(_ G1 : Gen, G2 : Gen) {
    G1.IntTaker(1, i1 : G2.#^MEMBER5^#
  }

  func f6(_ G1 : Gen, G2 : Gen) {
    G1.StringTaker("", s2: G2.#^MEMBER6^#
  }

  func f7(_ GA : [Gen]) {
    foo(1, b1 : GA.#^MEMBER7^#
  }

  func f8(_ GA : Gen) {
    foo(1, b1 : GA.IG.#^MEMBER8^#
  }

  func f9() {
    foo(1, b1 : GenGenerator(1).#^MEMBER9^#
  }

  func f10(_ G: Gen) {
    foo(G.#^FARG3^#
  }

  func f11(_ G: Gen) {
    bar(G.#^FARG4^#
  }

  func f12(_ G1 : Gen, G2 : Gen) {
    G1.IntTaker(G2.#^FARG5^#
  }

  func f13(_ G : Gen) {
    G.IntTaker(G.IG.#^FARG6^#
  }
}

// MEMBER1: Begin completions
// MEMBER1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntGen()[#Int#]; name=IntGen()
// MEMBER1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: IntOpGen()[#Int?#]; name=IntOpGen()
// MEMBER1-DAG: Decl[InstanceMethod]/CurrNominal:   StringGen()[#String#]; name=StringGen()
// MEMBER1-DAG: Decl[InstanceMethod]/CurrNominal:   StringOpGen()[#String?#]; name=StringOpGen()
// MEMBER1-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: IntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]; name=IntTaker(i1: Int, i2: Int)
// MEMBER1-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: StringTaker({#(s1): String#}, {#s2: String#})[#Void#]; name=StringTaker(s1: String, s2: String)

// MEMBER2: Begin completions
// MEMBER2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: IntGen()[#Int#]; name=IntGen()
// MEMBER2-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]; name=IntOpGen()
// MEMBER2-DAG: Decl[InstanceMethod]/CurrNominal:   StringGen()[#String#]; name=StringGen()
// MEMBER2-DAG: Decl[InstanceMethod]/CurrNominal:   StringOpGen()[#String?#]; name=StringOpGen()
// MEMBER2-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: IntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]; name=IntTaker(i1: Int, i2: Int)
// MEMBER2-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: StringTaker({#(s1): String#}, {#s2: String#})[#Void#]; name=StringTaker(s1: String, s2: String)

// MEMBER3: Begin completions
// MEMBER3-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]; name=IntGen()
// MEMBER3-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]; name=IntOpGen()
// MEMBER3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: StringGen()[#String#]; name=StringGen()
// MEMBER3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: StringOpGen()[#String?#]; name=StringOpGen()
// MEMBER3-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: IntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]; name=IntTaker(i1: Int, i2: Int)
// MEMBER3-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: StringTaker({#(s1): String#}, {#s2: String#})[#Void#]; name=StringTaker(s1: String, s2: String)

// MEMBER4: Begin completions
// MEMBER4-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]; name=IntGen()
// MEMBER4-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]; name=IntOpGen()
// MEMBER4-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: StringGen()[#String#]; name=StringGen()
// MEMBER4-DAG: Decl[InstanceMethod]/CurrNominal:   StringOpGen()[#String?#]; name=StringOpGen()
// MEMBER4-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: IntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]; name=IntTaker(i1: Int, i2: Int)
// MEMBER4-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: StringTaker({#(s1): String#}, {#s2: String#})[#Void#]; name=StringTaker(s1: String, s2: String)

// MEMBER7: Begin completions
// MEMBER7-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: removeAll()[#Void#]; name=removeAll()
// MEMBER7-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: removeAll({#keepingCapacity: Bool#})[#Void#]; name=removeAll(keepingCapacity: Bool)
// MEMBER7-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: count[#Int#]; name=count
// MEMBER7-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: capacity[#Int#]; name=capacity

// MEMBER8: Begin completions
// MEMBER8-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: InternalIntGen()[#Int#]; name=InternalIntGen()
// MEMBER8-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: InternalIntOpGen()[#Int?#]; name=InternalIntOpGen()
// MEMBER8-DAG: Decl[InstanceMethod]/CurrNominal:   InternalStringGen()[#String#]; name=InternalStringGen()
// MEMBER8-DAG: Decl[InstanceMethod]/CurrNominal:   InternalStringOpGen()[#String?#]; name=InternalStringOpGen()
// MEMBER8-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: InternalIntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]; name=InternalIntTaker(i1: Int, i2: Int)
// MEMBER8-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: InternalStringTaker({#(s1): String#}, {#s2: String#})[#Void#]; name=InternalStringTaker(s1: String, s2: String)

// FARG6: Begin completions
// FARG6-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: InternalIntGen()[#Int#]
// FARG6-DAG: Decl[InstanceMethod]/CurrNominal:   InternalIntOpGen()[#Int?#]
// FARG6-DAG: Decl[InstanceMethod]/CurrNominal:   InternalStringGen()[#String#]
// FARG6-DAG: Decl[InstanceMethod]/CurrNominal:   InternalStringOpGen()[#String?#]
// FARG6-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: InternalIntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]
// FARG6-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: InternalStringTaker({#(s1): String#}, {#s2: String#})[#Void#]

func firstArg(arg1 arg1: Int, arg2: Int) {}
func testArg1Name1() {
  firstArg(#^FIRST_ARG_NAME_1^#
}
// FIRST_ARG_NAME_PATTERN: ['(']{#arg1: Int#}, {#arg2: Int#}[')']
func testArg2Name1() {
  firstArg(#^FIRST_ARG_NAME_2^#)
}

func testArg2Name3() {
  firstArg(#^FIRST_ARG_NAME_3^#,
}
// FIRST_ARG_NAME_3: Keyword/ExprSpecific: arg1: [#Argument name#]
// FIRST_ARG_NAME_4: Pattern/CurrModule: ['(']{#arg1: Int#}, {#arg2: Int#}[')'][#Void#];

func takeArray<T>(_ x: [T]) {}
struct TestBoundGeneric1 {
  let x: [Int]
  let y: [Int]
  func test1() {
    takeArray(self.#^BOUND_GENERIC_1_1^#)
  }
  func test2() {
    takeArray(#^BOUND_GENERIC_1_2^#)
  }
// FIXME: These should be convertible to [T]. rdar://problem/24570603
// BOUND_GENERIC_1: Decl[InstanceVar]/CurrNominal:      x[#[Int]#];
// BOUND_GENERIC_1: Decl[InstanceVar]/CurrNominal:      y[#[Int]#];
}

func whereConvertible<T>(lhs: T, rhs: T) where T: Collection {
  _ = zip(lhs, #^GENERIC_TO_GENERIC^#)
}
// GENERIC_TO_GENERIC: Begin completions
// GENERIC_TO_GENERIC: Decl[LocalVar]/Local: lhs[#Collection#]; name=lhs
// GENERIC_TO_GENERIC: Decl[LocalVar]/Local: rhs[#Collection#]; name=rhs
// GENERIC_TO_GENERIC: End completions

func emptyOverload() {}
func emptyOverload(foo foo: Int) {}
emptyOverload(foo: #^EMPTY_OVERLOAD_1^#)
struct EmptyOverload {
  init() {}
  init(foo: Int) {}
}
_ = EmptyOverload(foo: #^EMPTY_OVERLOAD_2^#)
// EMPTY_OVERLOAD: Begin completions
// EMPTY_OVERLOAD-DAG: Decl[GlobalVar]/Local/TypeRelation[Identical]: i2[#Int#];
// EMPTY_OVERLOAD-DAG: Decl[GlobalVar]/Local/TypeRelation[Identical]: i1[#Int#];
// EMPTY_OVERLOAD: End completions

public func fopen() -> TestBoundGeneric1! { fatalError() }
func other() {
  _ = fopen(#^CALLARG_IUO^#)
// CALLARG_IUO-NOT: Begin completions
// CALLARG_IUO-NOT: End completions
}

class Foo { let x: Int }
class Bar {
  var collectionView: Foo!

  func foo() {
    self.collectionView? .#^BOUND_IUO^#x
    self.collectionView! .#^FORCED_IUO^#x
  }
  // MEMBEROF_IUO: Begin completions, 2 items
  // MEMBEROF_IUO: Keyword[self]/CurrNominal: self[#Foo#]; name=self
  // MEMBEROF_IUO: Decl[InstanceVar]/CurrNominal: x[#Int#]; name=x
  // MEMBEROF_IUO: End completions
}

func curry<T1, T2, R>(_ f: @escaping (T1, T2) -> R) -> (T1) -> (T2) -> R {
  return { t1 in { t2 in f(#^NESTED_CLOSURE^#, t2) } }
  // NESTED_CLOSURE: Begin completions
  // FIXME: Should be '/TypeRelation[Invalid]: t2[#T2#]'
  // NESTED_CLOSURE: Decl[LocalVar]/Local:               t2; name=t2
  // NESTED_CLOSURE: Decl[LocalVar]/Local:               t1[#T1#]; name=t1
}

func trailingClosureLocal(x: Int, fn: (Int) -> Void) {
  trailingClosureLocal(x: 1) { localArg in
    var localVar = 1
    if #^TRAILING_CLOSURE_LOCAL^#
  }
  // TRAILING_CLOSURE_LOCAL: Begin completions
  // TRAILING_CLOSURE_LOCAL: Decl[LocalVar]/Local: localArg[#Int#]; name=localArg
  // TRAILING_CLOSURE_LOCAL: Decl[LocalVar]/Local: localVar[#Int#]; name=localVar
}

func shuffled(_ x: Int ..., y: String = "", z: SimpleEnum = .foo) {}
func testTupleShuffle() {
  let _ = shuffled(1, 2, 3, 4, #^SHUFFLE_1^#, z: .foo)
  let _ = shuffled(1, 2, 3, y: #^SHUFFLE_2^#
  let _ = shuffled(z: .#^SHUFFLE_3^#)
}
// SHUFFLE_1: Begin completions
// SHUFFLE_1-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: i1[#Int#]; name=i1
// SHUFFLE_1-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: i2[#Int#]; name=i2

// SHUFFLE_2: Begin completions
// SHUFFLE_2-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: s1[#String#]; name=s1
// SHUFFLE_2-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: s2[#String#]; name=s2

// SHUFFLE_3: Begin completions, 3 items
// SHUFFLE_3-DAG: Decl[EnumElement]/ExprSpecific:     foo[#SimpleEnum#]; name=foo
// SHUFFLE_3-DAG: Decl[EnumElement]/ExprSpecific:     bar[#SimpleEnum#]; name=bar
// SHUFFLE_3-DAG: Decl[EnumElement]/ExprSpecific:     baz[#SimpleEnum#]; name=baz

class HasSubscript {
  subscript(idx: Int) -> String {}
  subscript(idx: Int, default defaultValue: String) -> String {}
}
func testSubscript(obj: HasSubscript, intValue: Int, strValue: String) {
  let _ = obj[#^SUBSCRIPT_1^#
// SUBSCRIPT_1: Begin completions
// SUBSCRIPT_1-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: i1[#Int#]; name=i1
// SUBSCRIPT_1-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: i2[#Int#]; name=i2
// SUBSCRIPT_1-DAG: Decl[GlobalVar]/CurrModule: s1[#String#]; name=s1
// SUBSCRIPT_1-DAG: Decl[GlobalVar]/CurrModule: s2[#String#]; name=s2

  let _ = obj[.#^SUBSCRIPT_1_DOT^#
// SUBSCRIPT_1_DOT: Begin completions
// SUBSCRIPT_1_DOT-NOT: i1
// SUBSCRIPT_1_DOT-NOT: s1
// SUBSCRIPT_1_DOT-DAG: Decl[StaticVar]/Super:              max[#Int#]; name=max
// SUBSCRIPT_1_DOT-DAG: Decl[StaticVar]/Super:              min[#Int#]; name=min

  let _ = obj[42, #^SUBSCRIPT_2^#
// SUBSCRIPT_2: Begin completions, 1 items
// SUBSCRIPT_2-NEXT: Keyword/ExprSpecific:               default: [#Argument name#]; name=default: 

  let _ = obj[42, .#^SUBSCRIPT_2_DOT^#
// SUBSCRIPT_2_DOT-NOT: Begin completions

  let _ = obj[42, default: #^SUBSCRIPT_3^#
// SUBSCRIPT_3: Begin completions
// SUBSCRIPT_3-DAG: Decl[GlobalVar]/CurrModule: i1[#Int#]; name=i1
// SUBSCRIPT_3-DAG: Decl[GlobalVar]/CurrModule: i2[#Int#]; name=i2
// SUBSCRIPT_3-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: s1[#String#]; name=s1
// SUBSCRIPT_3-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: s2[#String#]; name=s2

  let _ = obj[42, default: .#^SUBSCRIPT_3_DOT^#
// SUBSCRIPT_3_DOT: Begin completions
// SUBSCRIPT_3_DOT-NOT: i1
// SUBSCRIPT_3_DOT-NOT: s1
// SUBSCRIPT_3_DOT-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]: init()[#String#]; name=init()
// SUBSCRIPT_3_DOT-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]: init({#(c): Character#})[#String#]; name=init(c: Character)

}
