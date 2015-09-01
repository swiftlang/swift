// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG1 | FileCheck %s -check-prefix=EXPECT_OINT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG2 | FileCheck %s -check-prefix=ARG-NAME1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG3 | FileCheck %s -check-prefix=ARG-NAME2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG4 | FileCheck %s -check-prefix=EXPECT_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG5 | FileCheck %s -check-prefix=EXPECT_OSTRING
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG6 | FileCheck %s -check-prefix=ARG-NAME2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG7 | FileCheck %s -check-prefix=ARG-NAME1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARG8 | FileCheck %s -check-prefix=EXPECT_STRING

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD1 | FileCheck %s -check-prefix=OVERLOAD1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD2 | FileCheck %s -check-prefix=OVERLOAD2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD3 | FileCheck %s -check-prefix=OVERLOAD3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD4 | FileCheck %s -check-prefix=OVERLOAD4

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
func foo(a : Int) {}
func foo(a : Int, b1 :Int?) {}
func foo(a : Int, b2 :Int?, b3: Int?) {}
func foo1(a : Int, b : Int) {}
func bar(a : String, b : String?) {}
func bar1(a : String, b1 : String) {}
func bar1(a : String, b2 : String) {}

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
}

// ARG-NAME1: Begin completions, 2 items
// ARG-NAME1-DAG: Keyword/ExprSpecific:               b1: [#Argument name#]; name=b1:
// ARG-NAME1-DAG: Keyword/ExprSpecific:               b2: [#Argument name#]; name=b2:

// ARG-NAME2: Begin completions, 1 items
// ARG-NAME2-DAG: Keyword/ExprSpecific:               b: [#Argument name#]; name=b:

// EXPECT_OINT: Begin completions
// EXPECT_OINT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_OINT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_OINT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_OINT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: i2[#Int#]; name=i2
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: i1[#Int#]; name=i1
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: oi2[#Int?#]; name=oi2
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: oi1[#Int?#]; name=oi1
// EXPECT_OINT-DAG: Decl[GlobalVar]/CurrModule:         os1[#String?#]; name=os1

// EXPECT_INT: Begin completions
// EXPECT_INT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_INT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_INT-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Invalid]: voidGen()[#Void#]; name=voidGen()
// EXPECT_INT-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: intGen()[#Int#]; name=intGen()
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: i1[#Int#]; name=i1
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: i2[#Int#]; name=i2
// EXPECT_INT-DAG: Decl[FreeFunction]/CurrModule:      ointGen()[#Int?#]; name=ointGen()
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule:         oi1[#Int?#]; name=oi1
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule:         os2[#String?#]; name=os2
// EXPECT_INT-DAG: Decl[GlobalVar]/CurrModule:         oi2[#Int?#]; name=oi2

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
// EXPECT_OSTRING-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_OSTRING-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_OSTRING-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Convertible]: stringGen()[#String#]; name=stringGen()
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: s2[#String#]; name=s2
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: s1[#String#]; name=s1
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: os1[#String?#]; name=os1
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: os2[#String?#]; name=os2
// EXPECT_OSTRING-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: ostringGen()[#String?#]; name=ostringGen()
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule:         i1[#Int#]; name=i1
// EXPECT_OSTRING-DAG: Decl[GlobalVar]/CurrModule:         i2[#Int#]; name=i2

// EXPECT_STRING: Begin completions
// EXPECT_STRING-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// EXPECT_STRING-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2()[#Void#]; name=f2()
// EXPECT_STRING-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: stringGen()[#String#]; name=stringGen()
// EXPECT_STRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: s1[#String#]; name=s1
// EXPECT_STRING-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: s2[#String#]; name=s2
// EXPECT_STRING-DAG: Decl[GlobalVar]/CurrModule:         os1[#String?#]; name=os1
// EXPECT_STRING-DAG: Decl[GlobalVar]/CurrModule:         os2[#String?#]; name=os2

func foo2(a : C1, b1 : C2) {}
func foo2(a : C2, b2 : C1) {}

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
// OVERLOAD3-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]: C2I[#C2#]; name=C2I
// OVERLOAD3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// OVERLOAD3-DAG: Decl[InstanceVar]/CurrNominal:      C1I[#C1#]; name=C1I

// OVERLOAD4: Begin completions
// OVERLOAD4-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]: C1I[#C1#]; name=C1I
// OVERLOAD4-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1()[#Void#]; name=f1()
// OVERLOAD4-DAG: Decl[InstanceVar]/CurrNominal:      C2I[#C2#]; name=C2I
