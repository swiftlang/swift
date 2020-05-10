// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOOP_1 | %FileCheck %s -check-prefix=LOOP_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOOP_2 > %t.loop2.txt
// RUN: %FileCheck %s -check-prefix=LOOP_2 < %t.loop2.txt
// RUN: %FileCheck %s -check-prefix=LOOP_2_NEGATIVE < %t.loop2.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOOP_3 | %FileCheck %s -check-prefix=LOOP_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOOP_4 | %FileCheck %s -check-prefix=LOOP_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOOP_5 | %FileCheck %s -check-prefix=LOOP_5

class Gen {
	func IntGen() -> Int { return 0 }
	func IntOpGen() -> Int? {return 0}
	func IntSeqGen() -> [Int] {return [0]}
}

class C {

	func f1(_ Seq : [Int], I : Int, G : Gen) {
		for i in #^LOOP_1^#
	}
// LOOP_1: Begin completions
// LOOP_1-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: Seq[#[Int]#]{{; name=.+$}}
// LOOP_1-DAG: Decl[LocalVar]/Local:               I[#Int#]{{; name=.+$}}
// LOOP_1-DAG: Decl[LocalVar]/Local:               G[#Gen#]{{; name=.+$}}
// LOOP_1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#]{{; name=.+$}}
// LOOP_1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#]{{; name=.+$}}

	func f2(_ Seq : [Int], I : Int, G: Gen) {
		for i in #^LOOP_2^# {

		}
	}
// LOOP_2: Begin completions
// LOOP_2-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: Seq[#[Int]#]{{; name=.+$}}
// LOOP_2-DAG: Decl[LocalVar]/Local:               I[#Int#]{{; name=.+$}}
// LOOP_2-DAG: Decl[LocalVar]/Local:               G[#Gen#]{{; name=.+$}}
// LOOP_2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#]{{; name=.+$}}
// LOOP_2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#]{{; name=.+$}}

// LOOP_2_NEGATIVE-NOT: TypeRelation[{{.*}}]: SequenceType[#SequenceType#];

	func f3(_ G : Gen) {
		for i in G.#^LOOP_3^#
	}

// LOOP_3: Begin completions
// LOOP_3-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]{{; name=.+$}}
// LOOP_3-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]{{; name=.+$}}
// LOOP_3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntSeqGen()[#[Int]#]{{; name=.+$}}

	func f4(_ G : Gen) {
		for i in G.#^LOOP_4^# {

		}
	}

// LOOP_4: Begin completions
// LOOP_4-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]{{; name=.+$}}
// LOOP_4-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]{{; name=.+$}}
// LOOP_4-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntSeqGen()[#[Int]#]{{; name=.+$}}

  func f5(_ a: [C]) {
    do {
      for user in a {
        user.#^LOOP_5^#
      }
    } catch {}
  }
// LOOP_5: Begin completions
}
