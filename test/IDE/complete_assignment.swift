// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_1 | FileCheck %s -check-prefix=ASSIGN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_2 | FileCheck %s -check-prefix=ASSIGN_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_3 | FileCheck %s -check-prefix=ASSIGN_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGN_4 | FileCheck %s -check-prefix=ASSIGN_4

class C1 {
var I1 = 1
var I2 = 3
var IO1 : Int?

var S1 = ""
var S2 = ""
var SO1 : String?

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

func f1() {
	var I3 : Int
	I3 = #^ASSIGN_1^#
}

// ASSIGN_1: Begin completions
// ASSIGN_1: Decl[LocalVar]/Local:               I3[#Int#]; name=I3
// ASSIGN_1: Decl[InstanceVar]/CurrNominal:      I1[#Int#]; name=I1
// ASSIGN_1: Decl[InstanceVar]/CurrNominal:      I2[#Int#]; name=I2
// ASSIGN_1: Decl[InstanceMethod]/CurrNominal:   IntGenerator()[#Int#]; name=IntGenerator()
// ASSIGN_1-NOT: ?

func f2() {
	var I3 : Int?
	I3 = #^ASSIGN_2^#
}

// ASSIGN_2: Begin completions
// ASSIGN_2: Decl[LocalVar]/Local:               I3[#Int?#]; name=I3
// ASSIGN_2: Decl[InstanceVar]/CurrNominal:      I1[#Int#]; name=I1
// ASSIGN_2: Decl[InstanceVar]/CurrNominal:      I2[#Int#]; name=I2
// ASSIGN_2: Decl[InstanceVar]/CurrNominal:      IO1[#Int?#]; name=IO1
// ASSIGN_2: Decl[InstanceMethod]/CurrNominal:   IntGenerator()[#Int#]; name=IntGenerator()
// ASSIGN_2: Decl[InstanceMethod]/CurrNominal:   IntOpGenerator()[#Int?#]; name=IntOpGenerator()

	func f3() {
		var S3 = ""
		S3 = #^ASSIGN_3^#
	}

// ASSIGN_3: Begin completions
// ASSIGN_3: Decl[LocalVar]/Local:               S3[#String#]; name=S3
// ASSIGN_3: Decl[InstanceVar]/CurrNominal:      S1[#String#]; name=S1
// ASSIGN_3: Decl[InstanceVar]/CurrNominal:      S2[#String#]; name=S2
// ASSIGN_3: Decl[InstanceMethod]/CurrNominal:   StringGenerator()[#String#]; name=StringGenerator()
// ASSIGN_3-NOT: ?
// ASSIGN_3-NOT: Int

	func f4() {
		var S4 : String?
		S4 = #^ASSIGN_4^#
	}

// ASSIGN_4: Begin completions
// ASSIGN_4: Decl[LocalVar]/Local:               S4[#String?#]; name=S4
// ASSIGN_4: Decl[InstanceVar]/CurrNominal:      S1[#String#]; name=S1
// ASSIGN_4: Decl[InstanceVar]/CurrNominal:      S2[#String#]; name=S2
// ASSIGN_4: Decl[InstanceVar]/CurrNominal:      SO1[#String?#]; name=SO1
// ASSIGN_4: Decl[InstanceMethod]/CurrNominal:   StringGenerator()[#String#]; name=StringGenerator()
// ASSIGN_4: Decl[InstanceMethod]/CurrNominal:   StringOpGenerator()[#String?#]; name=StringOpGenerator()
// ASSIGN_4-NOT: Int
}
