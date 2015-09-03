// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_1 > %t.members1.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_1 < %t.members1.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members1.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_2B > %t.members2a.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_2 < %t.members2a.txt
// FIXME: filter?
// RUN-disabled: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members2a.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_2B > %t.members2b.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_2 < %t.members2b.txt
// FIXME: filter?
// RUN-disabled: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members2b.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_3 > %t.members3.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_3 < %t.members3.txt
// FIXME: filter?
// RUN-disabled: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members3.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_4 > %t.members4.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_4 < %t.members4.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members4.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_5 > %t.members5.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_5 < %t.members5.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members5.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_6 > %t.members6.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_6 < %t.members6.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members6.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_7 > %t.members7.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_7 < %t.members7.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members7.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_8 > %t.members8.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_8 < %t.members8.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members8.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_9 > %t.members9.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_9 < %t.members9.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members9.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURNS_ANY_SEQUENCE | FileCheck %s -check-prefix=RETURNS_ANY_SEQUENCE

// NO_STDLIB_PRIVATE: Begin completions
// NO_STDLIB_PRIVATE-NOT: Decl[{{.*}}]{{[^:]*}}: _
// NO_STDLIB_PRIVATE: End completions

#^PLAIN_TOP_LEVEL_1^#

// PLAIN_TOP_LEVEL: Begin completions
// PLAIN_TOP_LEVEL-DAG: Decl[Struct]/OtherModule[Swift]: Array[#Array#]{{; name=.+$}}
// PLAIN_TOP_LEVEL: End completions

func privateNominalMembers(a: String) {
  a.#^PRIVATE_NOMINAL_MEMBERS_1^#
}

// PRIVATE_NOMINAL_MEMBERS_1: Begin completions

// FIXME: we should show the qualified String.Index type.
// rdar://problem/20788802
// PRIVATE_NOMINAL_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal: startIndex[#Index#]{{; name=.+$}}
// PRIVATE_NOMINAL_MEMBERS_1: End completions

func protocolExtCollection1a<C : CollectionType>(a: C) {
  a.#^PRIVATE_NOMINAL_MEMBERS_2A^#
}

func protocolExtCollection1b(a: CollectionType) {
  a.#^PRIVATE_NOMINAL_MEMBERS_2B^#
}

// PRIVATE_NOMINAL_MEMBERS_2: Begin completions
// PRIVATE_NOMINAL_MEMBERS_2-DAG: map({#(transform): Self.Generator.Element throws -> T##Self.Generator.Element throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_2-DAG-NOT: Decl{{.*}}: last
// PRIVATE_NOMINAL_MEMBERS_2: End completions

func protocolExtCollection2<C : CollectionType where C.Index : BidirectionalIndexType>(a: C) {
  a.#^PRIVATE_NOMINAL_MEMBERS_3^#
}

// PRIVATE_NOMINAL_MEMBERS_3: Begin completions
// PRIVATE_NOMINAL_MEMBERS_3-DAG: map({#(transform): C.Generator.Element throws -> T##C.Generator.Element throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3-DAG: Decl[InstanceVar]/Super:            last[#C.Generator.Element?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3-DAG-NOT: Decl{{.*}}:         indexOf({#({{.*}}): Self.Generator.Element
// PRIVATE_NOMINAL_MEMBERS_3-DAG: indexOf({#(predicate): C.Generator.Element throws -> Bool##C.Generator.Element throws -> Bool#})[' rethrows'][#C.Index?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3: End completions

func protocolExtArray<T : Equatable>(a: [T]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_4^#
}
// PRIVATE_NOMINAL_MEMBERS_4: Begin completions
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         map({#(transform): T throws -> T##T throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceVar]/Super:            last[#T?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         indexOf({#(element): T#})[#Int?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         indexOf({#(predicate): T throws -> Bool##T throws -> Bool#})[' rethrows'][#Int?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4: End completions

func testArchetypeReplacement1<FOO : Equatable>(a: [FOO]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_5^#
}

// PRIVATE_NOMINAL_MEMBERS_5: Begin completions
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): FOO#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/CurrNominal:   insert({#(newElement): FOO#}, {#atIndex: Int#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/CurrNominal:   popLast()[#FOO?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         generate()[#IndexingGenerator<[FOO]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceVar]/Super:            isEmpty[#Bool#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceVar]/Super:            first[#FOO?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         dropFirst({#(n): Int#})[#ArraySlice<FOO>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         dropLast({#(n): Int#})[#ArraySlice<FOO>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         prefix({#(maxLength): Int#})[#ArraySlice<FOO>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         suffix({#(maxLength): Int#})[#ArraySlice<FOO>#]{{; name=.+}}


func testArchetypeReplacement2<BAR : Equatable>(a: [BAR]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_6^#
}

// PRIVATE_NOMINAL_MEMBERS_6: Begin completions
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): BAR#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/CurrNominal:   insert({#(newElement): BAR#}, {#atIndex: Int#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/CurrNominal:   popLast()[#BAR?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         dropFirst()[#ArraySlice<BAR>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         dropLast()[#ArraySlice<BAR>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         enumerate()[#EnumerateSequence<[BAR]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         minElement({#(isOrderedBefore): (BAR, BAR) throws -> Bool##(BAR, BAR) throws -> Bool#})[' rethrows'][#BAR?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         maxElement({#(isOrderedBefore): (BAR, BAR) throws -> Bool##(BAR, BAR) throws -> Bool#})[' rethrows'][#BAR?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         reduce({#(initial): T#}, {#combine: (T, BAR) throws -> T##(T, BAR) throws -> T#})[' rethrows'][#T#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         reverse()[#ReverseCollection<[BAR]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         reverse()[#ReverseRandomAccessCollection<[BAR]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         flatMap({#(transform): BAR throws -> S##BAR throws -> S#})[' rethrows'][#[S.Generator.Element]#]{{; name=.+}}

func testArchetypeReplacement3 (a : [Int]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_7^#
}

// PRIVATE_NOMINAL_MEMBERS_7: Begin completions
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): Int#})[#Void#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/CurrNominal:   removeLast()[#Int#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/CurrNominal:   popLast()[#Int?#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceVar]/Super:            first[#Int?#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         map({#(transform): Int throws -> T##Int throws -> T#})[' rethrows'][#[T]#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         dropLast({#(n): Int#})[#ArraySlice<Int>#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         dropFirst({#(n): Int#})[#AnySequence<Int>#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         prefix({#(maxLength): Int#})[#AnySequence<Int>#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         elementsEqual({#(other): OtherSequence#}, {#isEquivalent: (Int, Int) throws -> Bool##(Int, Int) throws -> Bool#})[' rethrows'][#Bool#]


protocol P2 {
  typealias MyElement
}

extension P2 {
  func foo(x: MyElement) {}
}

typealias MyInt = Int

class MyClass1 : P2 {
  typealias MyElement = MyInt
}

class MyClass2 : P2 {
  typealias MyElement = Int
}

func testArchetypeReplacement4(a : MyClass1) {
  a.#^PRIVATE_NOMINAL_MEMBERS_8^#
}
// PRIVATE_NOMINAL_MEMBERS_8: Begin completions
// PRIVATE_NOMINAL_MEMBERS_8-DAG: Decl[InstanceMethod]/Super: foo({#(x): MyInt#})[#Void#]{{; name=.+}}

func testArchetypeReplacement5(a : MyClass2) {
  a.#^PRIVATE_NOMINAL_MEMBERS_9^#
}

// PRIVATE_NOMINAL_MEMBERS_9: Begin completions
// PRIVATE_NOMINAL_MEMBERS_9-DAG: Decl[InstanceMethod]/Super: foo({#(x): Int#})[#Void#]{{; name=.+}}

// rdar://problem/22334700
struct Test1000 : SequenceType {
  func #^RETURNS_ANY_SEQUENCE^#
}
// RETURNS_ANY_SEQUENCE: Decl[InstanceMethod]/Super:         dropFirst(n: Int)
