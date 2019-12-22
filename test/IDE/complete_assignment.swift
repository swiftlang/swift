// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_1 | %FileCheck %s -check-prefix=ASSIGN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_2 | %FileCheck %s -check-prefix=ASSIGN_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_3 | %FileCheck %s -check-prefix=ASSIGN_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_4 | %FileCheck %s -check-prefix=ASSIGN_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_5 | %FileCheck %s -check-prefix=ASSIGN_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_6 | %FileCheck %s -check-prefix=ASSIGN_6

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_7 | %FileCheck %s -check-prefix=ASSIGN_7
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_8 | %FileCheck %s -check-prefix=ASSIGN_8
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_9 | %FileCheck %s -check-prefix=ASSIGN_9
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_10 | %FileCheck %s -check-prefix=ASSIGN_10
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_11 | %FileCheck %s -check-prefix=ASSIGN_11
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_12 | %FileCheck %s -check-prefix=ASSIGN_12

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_13 | %FileCheck %s -check-prefix=ASSIGN_13
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_14 | %FileCheck %s -check-prefix=ASSIGN_14
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_15 | %FileCheck %s -check-prefix=ASSIGN_15
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_16 | %FileCheck %s -check-prefix=ASSIGN_16
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_17 | %FileCheck %s -check-prefix=ASSIGN_17
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_18 | %FileCheck %s -check-prefix=ASSIGN_18
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_19 | %FileCheck %s -check-prefix=ASSIGN_19

class C1 {
var I1 = 1
var I2 = 3
var IO1 : Int?

var S1 = ""
var S2 = ""
var SO1 : String?

enum D1 {
  case case1
  case case2
}
enum D2 {
  case case3
  case case4
}

func IntGenerator() -> Int {
  return 0
}

func IntOpGenerator() -> Int? {
  return 0
}

func StringGenerator() -> String {
  return ""
}

func StringOpGenerator() -> String? {
  return ""
}

func VoidGen() {}

class C2 {
  func IntGen() -> Int { return 1 }
  func IntOpGen() -> Int? { return 1 }
  func D1Gen() -> D1 { return D1.case1 }
  func D2Gen() -> D2 { return D2.case3 }
  func VoidGen() {}
  var InternalC2 = C2()
}

func C2Gen() -> C2 { return C2() }

func f1() {
	var I3 : Int
	I3 = #^ASSIGN_1^#
}

// ASSIGN_1: Begin completions
// ASSIGN_1-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]:               	I3[#Int#]; name=I3
// ASSIGN_1-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:      	I1[#Int#]; name=I1
// ASSIGN_1-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:      	I2[#Int#]; name=I2
// ASSIGN_1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]:   	IntGenerator()[#Int#]; name=IntGenerator()
// ASSIGN_1-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: 			VoidGen()[#Void#]; name=VoidGen()
// ASSIGN_1-DAG: Decl[InstanceVar]/CurrNominal:                               S1[#String#]; name=S1

func f2() {
	var I3 : Int?
	I3 = #^ASSIGN_2^#
}

// ASSIGN_2-DAG: Begin completions
// ASSIGN_2-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]:               		I3[#Int?#]; name=I3
// ASSIGN_2-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]:      	I1[#Int#]; name=I1
// ASSIGN_2-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]:      	I2[#Int#]; name=I2
// ASSIGN_2-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:      		IO1[#Int?#]; name=IO1
// ASSIGN_2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]:   	IntGenerator()[#Int#]; name=IntGenerator()
// ASSIGN_2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]:      IntOpGenerator()[#Int?#]; name=IntOpGenerator()
// ASSIGN_2-DAG: Decl[InstanceVar]/CurrNominal:                                 S1[#String#]; name=S1

	func f3() {
		var S3 = ""
		S3 = #^ASSIGN_3^#
	}

// ASSIGN_3-DAG: Begin completions
// ASSIGN_3-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]:                S3[#String#]; name=S3
// ASSIGN_3-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:       S1[#String#]; name=S1
// ASSIGN_3-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:       S2[#String#]; name=S2
// ASSIGN_3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]:    StringGenerator()[#String#]; name=StringGenerator()
// ASSIGN_3-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]:      VoidGen()[#Void#]; name=VoidGen()
// ASSIGN_3-DAG: Decl[InstanceVar]/CurrNominal:                               I1[#Int#]; name=I1

	func f4() {
		var S4 : String?
		S4 = #^ASSIGN_4^#
	}

// ASSIGN_4-DAG: Begin completions
// ASSIGN_4-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]:                S4[#String?#]; name=S4
// ASSIGN_4-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]:     S1[#String#]; name=S1
// ASSIGN_4-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]:     S2[#String#]; name=S2
// ASSIGN_4-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:       SO1[#String?#]; name=SO1
// ASSIGN_4-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]:  StringGenerator()[#String#]; name=StringGenerator()
// ASSIGN_4-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]:    StringOpGenerator()[#String?#]; name=StringOpGenerator()
// ASSIGN_4-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]:      VoidGen()[#Void#]; name=VoidGen()
// ASSIGN_4-DAG: Decl[InstanceVar]/CurrNominal:                               I1[#Int#]; name=I1

	func f5() {
	  var d : D1
	  d = .#^ASSIGN_5^#
	}

// ASSIGN_5: Begin completions, 2 items
// ASSIGN_5-DAG: Decl[EnumElement]/ExprSpecific:     case2[#D1#]; name=case2
// ASSIGN_5-DAG: Decl[EnumElement]/ExprSpecific:     case1[#D1#]; name=case1

	func f6() {
	  var d : D2
	  d = .#^ASSIGN_6^#
	}
// ASSIGN_6: Begin completions, 2 items
// ASSIGN_6-DAG: Decl[EnumElement]/ExprSpecific:     case3[#D2#]; name=case3
// ASSIGN_6-DAG:Decl[EnumElement]/ExprSpecific:     case4[#D2#]; name=case4

  func f7 (C : C2) {
    var i : Int
    i = C.#^ASSIGN_7^#
  }

// ASSIGN_7: Begin completions
// ASSIGN_7-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: IntGen()[#Int#]
// ASSIGN_7-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]
// ASSIGN_7-DAG: Decl[InstanceMethod]/CurrNominal:   D1Gen()[#D1#]
// ASSIGN_7-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_7-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: VoidGen()[#Void#]
// ASSIGN_7-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]

  func f8 (C : C2) {
    var i : Int?
    i = C.#^ASSIGN_8^#
  }

// ASSIGN_8: Begin completions
// ASSIGN_8-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntGen()[#Int#]
// ASSIGN_8-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: IntOpGen()[#Int?#]
// ASSIGN_8-DAG: Decl[InstanceMethod]/CurrNominal:   D1Gen()[#D1#]
// ASSIGN_8-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_8-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: VoidGen()[#Void#]
// ASSIGN_8-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]

  func f9 (C : C2) {
    var d : D1
    d = C.#^ASSIGN_9^#
  }

// ASSIGN_9: Begin completions
// ASSIGN_9-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]
// ASSIGN_9-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]
// ASSIGN_9-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: D1Gen()[#D1#]
// ASSIGN_9-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_9-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: VoidGen()[#Void#]
// ASSIGN_9-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]

  func f10 (C : C2) {
    var d : D1
    d = C.InternalC2.#^ASSIGN_10^#
  }

// ASSIGN_10: Begin completions
// ASSIGN_10-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]
// ASSIGN_10-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]
// ASSIGN_10-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: D1Gen()[#D1#]
// ASSIGN_10-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_10-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: VoidGen()[#Void#]
// ASSIGN_10-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]

  func f11(C: C2) {
    d = C.#^ASSIGN_11^#
  }

// ASSIGN_11: Begin completions
// ASSIGN_11-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]
// ASSIGN_11-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]
// ASSIGN_11-DAG: Decl[InstanceMethod]/CurrNominal:   D1Gen()[#D1#]
// ASSIGN_11-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_11-DAG: Decl[InstanceMethod]/CurrNominal:   VoidGen()[#Void#]
// ASSIGN_11-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]

  func f12() {
    var i : Int
    i = C2Gen().#^ASSIGN_12^#
  }

// ASSIGN_12: Begin completions
// ASSIGN_12-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: IntGen()[#Int#]
// ASSIGN_12-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]
// ASSIGN_12-DAG: Decl[InstanceMethod]/CurrNominal:   D1Gen()[#D1#]
// ASSIGN_12-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_12-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: VoidGen()[#Void#]
// ASSIGN_12-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]

  func f13() {
    var i : Int = #^ASSIGN_13^#
  }
// ASSIGN_13: Begin completions
// ASSIGN_13-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:       I1[#Int#]; name=I1
// ASSIGN_13-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:       I2[#Int#]; name=I2
// ASSIGN_13-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]:    IntGenerator()[#Int#]; name=IntGenerator()
// ASSIGN_13-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]:      VoidGen()[#Void#]; name=VoidGen()
// ASSIGN_13-DAG: Decl[InstanceVar]/CurrNominal:                               S1[#String#]; name=S1

  func f14() {
    let i : Int? = #^ASSIGN_14^#
  }

// ASSIGN_14-DAG: Begin completions
// ASSIGN_14-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]:       I1[#Int#]; name=I1
// ASSIGN_14-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]:       I2[#Int#]; name=I2
// ASSIGN_14-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:         IO1[#Int?#]; name=IO1
// ASSIGN_14-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]:    IntGenerator()[#Int#]; name=IntGenerator()
// ASSIGN_14-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]:      IntOpGenerator()[#Int?#]; name=IntOpGenerator()
// ASSIGN_14-DAG: Decl[InstanceVar]/CurrNominal:                                 S1[#String#]; name=S1

  func f15() {
    let i = #^ASSIGN_15^#
  }
// ASSIGN_15-NOT: TypeRelation

  func f16 (C : C2) {
    var d : D1 = C.InternalC2.#^ASSIGN_16^#
  }

// ASSIGN_16: Begin completions
// ASSIGN_16-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]
// ASSIGN_16-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]
// ASSIGN_16-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: D1Gen()[#D1#]
// ASSIGN_16-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_16-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: VoidGen()[#Void#]
// ASSIGN_16-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]

  func f17() {
    var i : Int = C2Gen().#^ASSIGN_17^#
  }

// ASSIGN_17: Begin completions
// ASSIGN_17-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: IntGen()[#Int#]
// ASSIGN_17-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]
// ASSIGN_17-DAG: Decl[InstanceMethod]/CurrNominal:   D1Gen()[#D1#]
// ASSIGN_17-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_17-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: VoidGen()[#Void#]
// ASSIGN_17-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]

  func f18 (C : C2) {
    var d : D1 = C.#^ASSIGN_18^#
  }

// ASSIGN_18: Begin completions
// ASSIGN_18-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]
// ASSIGN_18-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]
// ASSIGN_18-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: D1Gen()[#D1#]
// ASSIGN_18-DAG: Decl[InstanceMethod]/CurrNominal:   D2Gen()[#D2#]
// ASSIGN_18-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: VoidGen()[#Void#]
// ASSIGN_18-DAG: Decl[InstanceVar]/CurrNominal:      InternalC2[#C2#]
}


class Test19 {
  func test() {
    let t = true
    prop = #^ASSIGN_19^#
  }
  var prop: Bool
}
// rdar://23111219
// ASSIGN_19: Decl[LocalVar]/Local/TypeRelation[Identical]: t[#Bool#]
