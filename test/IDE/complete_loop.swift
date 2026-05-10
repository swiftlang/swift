// FIXME: Fails with non-source-ordering
// RUN: %batch-code-completion -source-order-completion

class Gen {
  func IntGen() -> Int { return 0 }
  func IntOpGen() -> Int? {return 0}
  func IntSeqGen() -> [Int] {return [0]}
}

class C {

  func f1(_ Seq : [Int], I : Int, G : Gen) {
    for i in #^LOOP_1^#
  }
  // LOOP_1-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: Seq[#[Int]#]{{; name=.+$}}
  // LOOP_1-DAG: Decl[LocalVar]/Local:               I[#Int#]{{; name=.+$}}
  // LOOP_1-DAG: Decl[LocalVar]/Local:               G[#Gen#]{{; name=.+$}}
  // LOOP_1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#]{{; name=.+$}}
  // LOOP_1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#]{{; name=.+$}}

  func f2(_ Seq : [Int], I : Int, G: Gen) {
    for i in #^LOOP_2?check=LOOP_2;check=LOOP_2_NEGATIVE^# {

    }
  }
  // LOOP_2-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: Seq[#[Int]#]{{; name=.+$}}
  // LOOP_2-DAG: Decl[LocalVar]/Local:               I[#Int#]{{; name=.+$}}
  // LOOP_2-DAG: Decl[LocalVar]/Local:               G[#Gen#]{{; name=.+$}}
  // LOOP_2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f1({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#]{{; name=.+$}}
  // LOOP_2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: f2({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#]{{; name=.+$}}

  // LOOP_2_NEGATIVE-NOT: TypeRelation[{{.*}}]: SequenceType[#SequenceType#];

  func f3(_ G : Gen) {
    for i in G.#^LOOP_3^#
  }

  // LOOP_3-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]{{; name=.+$}}
  // LOOP_3-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]{{; name=.+$}}
  // LOOP_3-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntSeqGen()[#[Int]#]{{; name=.+$}}

  func f4(_ G : Gen) {
    for i in G.#^LOOP_4^# {

    }
  }

  // LOOP_4-DAG: Decl[InstanceMethod]/CurrNominal:   IntGen()[#Int#]{{; name=.+$}}
  // LOOP_4-DAG: Decl[InstanceMethod]/CurrNominal:   IntOpGen()[#Int?#]{{; name=.+$}}
  // LOOP_4-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: IntSeqGen()[#[Int]#]{{; name=.+$}}

  func f5(_ a: [C]) {
    do {
      for user in a {
        user.#^LOOP_5^#
        // LOOP_5-DAG: Keyword[self]/CurrNominal:          self[#C#];
        // LOOP_5-DAG: Decl[InstanceMethod]/CurrNominal:   f1({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#];
        // LOOP_5-DAG: Decl[InstanceMethod]/CurrNominal:   f2({#(Seq): [Int]#}, {#I: Int#}, {#G: Gen#})[#Void#];
        // LOOP_5-DAG: Decl[InstanceMethod]/CurrNominal:   f3({#(G): Gen#})[#Void#];
        // LOOP_5-DAG: Decl[InstanceMethod]/CurrNominal:   f4({#(G): Gen#})[#Void#];
        // LOOP_5-DAG: Decl[InstanceMethod]/CurrNominal:   f5({#(a): [C]#})[#Void#];
      }
    } catch {}
  }
}

// https://github.com/apple/swift/issues/58633
do {
  for value #^LOOP_6^#
}
// LOOP_6: Begin completions, 1 items
// LOOP_6-CHECK-NEXT: Keyword[in]/None:                   in; name=in

do {
  for value #^LOOP_7^# 1..<7 {}
}
// LOOP_7: Begin completions

// Pack Iteration
do {
  for t in #^LOOP_8^# {}
}
// LOOP_8-DAG: Keyword[repeat]/None:                   repeat; name=repeat

do {
  struct S {
    var foo: Bool
  }
  func bar(_ xs: [S]) {
    // Make sure we can resolve 'x' here without causing a request cycle.
    for x in xs where x.foo {
      x.#^LOOP_9^#
      // LOOP_9-DAG: Decl[InstanceVar]/CurrNominal: foo[#Bool#]; name=foo
    }
  }
}
