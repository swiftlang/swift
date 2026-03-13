// RUN: %batch-code-completion

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

// RETURN_VOID_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
}

func testReturnInt1() {
  return #^RETURN_INT_1^#
// RETURN_INT_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
}

func testReturnInt2(_ fooObject: FooStruct) {
  return fooObject.#^RETURN_INT_2^#
// RETURN_INT_2: Begin completions, 2 items
// RETURN_INT_2-DAG: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// RETURN_INT_2-DAG: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
}

func testMisplacedTry() throws -> Int {
  try return #^TRY_RETURN_INT?check=RETURN_INT_1^#
}

func testMisplacedTryVoid() throws {
  try return #^TRY_RETURN_VOID?check=RETURN_VOID_1^#
}

func testTR1() -> Int? {
	var i : Int
	var oi : Int?
	var fs : FooStruct
	return #^RETURN_TR1^#

// RETURN_TR1-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: oi[#Int?#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Convertible]: testTR1()[#Int?#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: i[#Int#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Invalid]: testReturnInt1()[#Void#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[LocalVar]/Local:               fs[#FooStruct#]{{; name=.+$}}
}

func testTR2(_ g : Gen) -> Int? {
  return g.#^RETURN_TR2^#
}

// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntGen()[#Int#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntOpGen()[#Int?#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal:   StringGen()[#String#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal:   StringOpGen()[#String?#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: IntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]{{; name=.+$}}
// RETURN_TR2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: StringTaker({#(s1): String#}, {#s2: String#})[#Void#]{{; name=.+$}}

func testTR3(_ g : Gen) -> Int? {
  return g.IG.#^RETURN_TR3^#
}

// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: InternalIntGen()[#Int#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: InternalIntOpGen()[#Int?#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal:   InternalStringGen()[#String#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal:   InternalStringOpGen()[#String?#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: InternalIntTaker({#(i1): Int#}, {#i2: Int#})[#Void#]{{; name=.+$}}
// RETURN_TR3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: InternalStringTaker({#(s1): String#}, {#s2: String#})[#Void#]{{; name=.+$}}

struct TestStruct {
  func testTR1_method() -> Int? {
    var i : Int
    var oi : Int?
    var fs : FooStruct
    return #^RETURN_TR1_METHOD?check=RETURN_TR1^#
  }
  func testTR2_method(_ g : Gen) -> Int? {
    return g.#^RETURN_TR2_METHOD?check=RETURN_TR2^#
  }
  func testTR3_method(_ g : Gen) -> Int? {
    return g.IG.#^RETURN_TR3_METHOD?check=RETURN_TR3^#
  }

  static func testTR1_static() -> Int? {
    var i : Int
    var oi : Int?
    var fs : FooStruct
    return #^RETURN_TR1_STATICMETHOD?check=RETURN_TR1^#
  }
  static func testTR2_static(_ g : Gen) -> Int? {
    return g.#^RETURN_TR2_STATICMETHOD?check=RETURN_TR2^#
  }
  static func testTR3_static(_ g : Gen) -> Int? {
    return g.IG.#^RETURN_TR3_STATICMETHOD?check=RETURN_TR3^#
  }
}

func testClosures(_ g: Gen) {
  var i : Int
  var oi : Int?
  var fs : FooStruct

  _ = { () -> Int? in
    return #^RETURN_TR1_CLOSURE?check=RETURN_TR1^#
  }
  _ = { () -> Int? in
    return g.#^RETURN_TR2_CLOSURE?check=RETURN_TR2^#
  }
  _ = { () -> Int? in
    return g.IG.#^RETURN_TR3_CLOSURE?check=RETURN_TR3^#
  }
}

// Make sure we can do a completion in an out-of-place return
do {
  return TestStruct.#^COMPLETE_IN_INVALID_RETURN^#
  // COMPLETE_IN_INVALID_RETURN: Decl[StaticMethod]/CurrNominal: testTR1_static()[#Int?#]; name=testTR1_static()
  // COMPLETE_IN_INVALID_RETURN: Decl[StaticMethod]/CurrNominal: testTR2_static({#(g): Gen#})[#Int?#]; name=testTR2_static(:)
  // COMPLETE_IN_INVALID_RETURN: Decl[StaticMethod]/CurrNominal: testTR3_static({#(g): Gen#})[#Int?#]; name=testTR3_static(:)
}

struct TestReturnInInit {
  init() {
    return TestStruct.#^COMPLETE_IN_INVALID_INIT_RETURN?check=COMPLETE_IN_INVALID_RETURN^#
  }
}
