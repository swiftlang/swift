// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_1 | FileCheck %s -check-prefix=ASSIGN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_2 | FileCheck %s -check-prefix=ASSIGN_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_3 | FileCheck %s -check-prefix=ASSIGN_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_4 | FileCheck %s -check-prefix=ASSIGN_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_5 | FileCheck %s -check-prefix=ASSIGN_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_6 | FileCheck %s -check-prefix=ASSIGN_6

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

func f1() {
	var I3 : Int
	I3 = #^ASSIGN_1^#
}

// ASSIGN_1: Begin completions
// ASSIGN_1-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]:               	I3[#Int#]; name=I3
// ASSIGN_1-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:      	I1[#Int#]; name=I1
// ASSIGN_1-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]:      	I2[#Int#]; name=I2
// ASSIGN_1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]:   	IntGenerator()[#Int#]; name=IntGenerator()
// ASSIGN_1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: 			VoidGen()[#Void#]; name=VoidGen()
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
// ASSIGN_3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]:      VoidGen()[#Void#]; name=VoidGen()
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
// ASSIGN_4-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]:      VoidGen()[#Void#]; name=VoidGen()
// ASSIGN_4-DAG: Decl[InstanceVar]/CurrNominal:                               I1[#Int#]; name=I1

	func f5() {
	  var d : D1
	  d = .#^ASSIGN_5^#
	}

// ASSIGN_5: Begin completions, 2 items
// ASSIGN_5-DAG: Decl[EnumElement]/ExprSpecific:     case2[#C1.D1#]; name=case2
// ASSIGN_5-DAG: Decl[EnumElement]/ExprSpecific:     case1[#C1.D1#]; name=case1

	func f6() {
	  var d : D2
	  d = .#^ASSIGN_6^#
	}
// ASSIGN_6: Begin completions, 2 items
// ASSIGN_6-DAG: Decl[EnumElement]/ExprSpecific:     case3[#C1.D2#]; name=case3
// ASSIGN_6-DAG:Decl[EnumElement]/ExprSpecific:     case4[#C1.D2#]; name=case4

}
