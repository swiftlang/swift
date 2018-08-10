// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_VOID_1 | %FileCheck %s -check-prefix=RETURN_VOID_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_INT_1 | %FileCheck %s -check-prefix=RETURN_INT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_INT_2 | %FileCheck %s -check-prefix=RETURN_INT_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TRY_RETURN_INT | %FileCheck %s -check-prefix=RETURN_INT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TRY_RETURN_VOID | %FileCheck %s -check-prefix=RETURN_VOID_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR1 | %FileCheck %s -check-prefix=RETURN_TR1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR2 | %FileCheck %s -check-prefix=RETURN_TR2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR3 | %FileCheck %s -check-prefix=RETURN_TR3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR1_METHOD | %FileCheck %s -check-prefix=RETURN_TR1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR2_METHOD | %FileCheck %s -check-prefix=RETURN_TR2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR3_METHOD | %FileCheck %s -check-prefix=RETURN_TR3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR1_STATICMETHOD | %FileCheck %s -check-prefix=RETURN_TR1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR2_STATICMETHOD | %FileCheck %s -check-prefix=RETURN_TR2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR3_STATICMETHOD | %FileCheck %s -check-prefix=RETURN_TR3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR1_CLOSURE | %FileCheck %s -check-prefix=RETURN_TR1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR2_CLOSURE | %FileCheck %s -check-prefix=RETURN_TR2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR3_CLOSURE | %FileCheck %s -check-prefix=RETURN_TR3

struct FooStruct {
  var instanceVar : Int
}

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

func testReturnVoid1() {
  return #^RETURN_VOID_1^#

// It is questionable if we should provide any results in a function returning
// Void.  But, the compiler allows us to put an expression of type Void here.
// A similar construct is also allowed in C, and might be used to cause a
// compiler error if the type of that expression changes to non-void.

// RETURN_VOID_1: Begin completions
// RETURN_VOID_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// RETURN_VOID_1: End completions
}

func testReturnInt1() {
  return #^RETURN_INT_1^#
// RETURN_INT_1: Begin completions
// RETURN_INT_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// RETURN_INT_1: End completions
}

func testReturnInt2(_ fooObject: FooStruct) {
  return fooObject.#^RETURN_INT_2^#
// RETURN_INT_2: Begin completions
// RETURN_INT_2-NEXT: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// RETURN_INT_2-NEXT: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
// RETURN_INT_2-NEXT: End completions
}

func testMisplacedTry() throws -> Int {
  try return #^TRY_RETURN_INT^#
}

func testMisplacedTryVoid() throws {
  try return #^TRY_RETURN_VOID^#
}

func testTR1() -> Int? {
	var i : Int
	var oi : Int?
	var fs : FooStruct
	return #^RETURN_TR1^#

// RETURN_TR1: Begin completions
// RETURN_TR1-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]: oi[#Int?#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: testTR1()[#Int?#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: i[#Int#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[FreeFunction]/CurrModule/NotRecommended/TypeRelation[Invalid]: testReturnInt1()[#Void#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[LocalVar]/Local:               fs[#FooStruct#]{{; name=.+$}}
}

func testTR2(_ g : Gen) -> Int? {
  return g.#^RETURN_TR2^#
}

// RETURN_TR2: Begin completions
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntGen()[#Int#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: IntOpGen()[#Int?#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal:   StringGen()[#String#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal:   StringOpGen()[#String?#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: IntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: StringTaker({#(s1): String#}, {#s2: String#})[#Void#]{{; name=.+$}}

func testTR3(_ g : Gen) -> Int? {
  return g.IG.#^RETURN_TR3^#
}

// RETURN_TR3: Begin completions
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: InternalIntGen()[#Int#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: InternalIntOpGen()[#Int?#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal:   InternalStringGen()[#String#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal:   InternalStringOpGen()[#String?#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: InternalIntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: InternalStringTaker({#(s1): String#}, {#s2: String#})[#Void#]{{; name=.+$}}

struct TestStruct {
  func testTR1_method() -> Int? {
    var i : Int
    var oi : Int?
    var fs : FooStruct
    return #^RETURN_TR1_METHOD^#
  }
  func testTR2_method(_ g : Gen) -> Int? {
    return g.#^RETURN_TR2_METHOD^#
  }
  func testTR3_method(_ g : Gen) -> Int? {
    return g.IG.#^RETURN_TR3_METHOD^#
  }

  static func testTR1_static() -> Int? {
    var i : Int
    var oi : Int?
    var fs : FooStruct
    return #^RETURN_TR1_STATICMETHOD^#
  }
  static func testTR2_static(_ g : Gen) -> Int? {
    return g.#^RETURN_TR2_STATICMETHOD^#
  }
  static func testTR3_static(_ g : Gen) -> Int? {
    return g.IG.#^RETURN_TR3_STATICMETHOD^#
  }
}

func testClosures(_ g: Gen) {
  var i : Int
  var oi : Int?
  var fs : FooStruct

  _ = { () -> Int? in
    return #^RETURN_TR1_CLOSURE^#
  }
  _ = { () -> Int? in
    return g.#^RETURN_TR2_CLOSURE^#
  }
  _ = { () -> Int? in
    return g.IG.#^RETURN_TR3_CLOSURE^#
  }
}
