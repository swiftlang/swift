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

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_10 > %t.members10.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_10 < %t.members10.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members10.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURNS_ANY_SEQUENCE | FileCheck %s -check-prefix=RETURNS_ANY_SEQUENCE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_INT_1 | FileCheck %s -check-prefix=POSTFIX_RVALUE_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_INT_2 | FileCheck %s -check-prefix=POSTFIX_LVALUE_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_OPTIONAL_1 | FileCheck %s -check-prefix=POSTFIX_OPTIONAL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INFIX_INT_1 | FileCheck %s -check-prefix=INFIX_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INFIX_INT_2 | FileCheck %s -check-prefix=INFIX_LVALUE_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INFIX_STRING_1 | FileCheck %s -check-prefix=INFIX_STRING
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INFIX_EXT_STRING_1 | FileCheck %s -check-prefix=INFIX_EXT_STRING

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
// PRIVATE_NOMINAL_MEMBERS_2-DAG: map({#(transform): (Self.Generator.Element) throws -> T##(Self.Generator.Element) throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_2-DAG-NOT: Decl{{.*}}: last
// PRIVATE_NOMINAL_MEMBERS_2: End completions

func protocolExtCollection2<C : CollectionType where C.Index : BidirectionalIndexType>(a: C) {
  a.#^PRIVATE_NOMINAL_MEMBERS_3^#
}

// PRIVATE_NOMINAL_MEMBERS_3: Begin completions
// PRIVATE_NOMINAL_MEMBERS_3-DAG: Decl[InstanceMethod]/Super:         map({#(transform): (C.Generator.Element) throws -> T##(C.Generator.Element) throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3-DAG: Decl[InstanceVar]/Super:            last[#C.Generator.Element?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3-DAG-NOT: Decl{{.*}}:         indexOf({#({{.*}}): Self.Generator.Element
// PRIVATE_NOMINAL_MEMBERS_3-DAG: indexOf({#(predicate): (C.Generator.Element) throws -> Bool##(C.Generator.Element) throws -> Bool#})[' rethrows'][#C.Index?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3: End completions

func protocolExtArray<T : Equatable>(a: [T]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_4^#
}
// PRIVATE_NOMINAL_MEMBERS_4: Begin completions
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         map({#(transform): (Equatable) throws -> T##(Equatable) throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceVar]/Super:            last[#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         indexOf({#(element): Equatable#})[#Int?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         indexOf({#(predicate): (Equatable) throws -> Bool##(Equatable) throws -> Bool#})[' rethrows'][#Int?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4: End completions

func testArchetypeReplacement1<FOO : Equatable>(a: [FOO]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_5^#
}

// PRIVATE_NOMINAL_MEMBERS_5: Begin completions
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): Equatable#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/CurrNominal:   insert({#(newElement): Equatable#}, {#atIndex: Int#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/CurrNominal:   popLast()[#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         generate()[#IndexingGenerator<[Equatable]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceVar]/Super:            isEmpty[#Bool#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceVar]/Super:            first[#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         dropFirst({#(n): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         dropLast({#(n): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         prefix({#(maxLength): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         suffix({#(maxLength): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}


func testArchetypeReplacement2<BAR : Equatable>(a: [BAR]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_6^#
}

// PRIVATE_NOMINAL_MEMBERS_6: Begin completions
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): Equatable#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/CurrNominal:   insert({#(newElement): Equatable#}, {#atIndex: Int#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/CurrNominal:   popLast()[#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         dropFirst()[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         dropLast()[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         enumerate()[#EnumerateSequence<[Equatable]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         minElement({#(isOrderedBefore): (Equatable, Equatable) throws -> Bool##(Equatable, Equatable) throws -> Bool#})[' rethrows'][#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         maxElement({#(isOrderedBefore): (Equatable, Equatable) throws -> Bool##(Equatable, Equatable) throws -> Bool#})[' rethrows'][#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         reduce({#(initial): T#}, {#combine: (T, Equatable) throws -> T##(T, Equatable) throws -> T#})[' rethrows'][#T#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         reverse()[#ReverseCollection<[Equatable]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         reverse()[#ReverseRandomAccessCollection<[Equatable]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         flatMap({#(transform): (Equatable) throws -> SequenceType##(Equatable) throws -> SequenceType#})[' rethrows'][#[S.Generator.Element]#]{{; name=.+}}

func testArchetypeReplacement3 (a : [Int]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_7^#
}

// PRIVATE_NOMINAL_MEMBERS_7: Begin completions
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): Int#})[#Void#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/CurrNominal:   removeLast()[#Int#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/CurrNominal:   popLast()[#Int?#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceVar]/Super:            first[#Int?#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         map({#(transform): (Int) throws -> T##(Int) throws -> T#})[' rethrows'][#[T]#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         dropLast({#(n): Int#})[#ArraySlice<Int>#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         dropFirst({#(n): Int#})[#AnySequence<Int>#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         prefix({#(maxLength): Int#})[#AnySequence<Int>#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         elementsEqual({#(other): SequenceType#}, {#isEquivalent: (Int, Int) throws -> Bool##(Int, Int) throws -> Bool#})[' rethrows'][#Bool#]


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

protocol P1{}

class MyClass3 {
  func foo<T: protocol<P1, P2>>(t : T) {}
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

func testArchetypeReplacement6() {
  var a = MyClass3()
  a.#^PRIVATE_NOMINAL_MEMBERS_10^#
}

// PRIVATE_NOMINAL_MEMBERS_10: Begin completions
// PRIVATE_NOMINAL_MEMBERS_10-DAG: Decl[InstanceMethod]/CurrNominal:   foo({#(t): protocol<P1, P2>#})[#Void#]{{; name=.+}}

// rdar://problem/22334700
struct Test1000 : SequenceType {
  func #^RETURNS_ANY_SEQUENCE^#
}
// RETURNS_ANY_SEQUENCE: Decl[InstanceMethod]/Super:         dropFirst(n: Int)

func testPostfixOperator1(x: Int) {
  x#^POSTFIX_INT_1^#
}
// POSTFIX_RVALUE_INT-NOT: ++
// POSTFIX_RVALUE_INT-NOT: --

func testPostfixOperator2(var x: Int) {
  x#^POSTFIX_INT_2^#
}
// POSTFIX_LVALUE_INT: Decl[PostfixOperatorFunction]/OtherModule[Swift]: ++[#Int#]; name=
// POSTFIX_LVALUE_INT: Decl[PostfixOperatorFunction]/OtherModule[Swift]: --[#Int#]; name=

func testPostfixOperator3(x: MyInt??) {
  x#^POSTFIX_OPTIONAL_1^#
}
// POSTFIX_OPTIONAL: Pattern/None: ![#MyInt?#]; name=!

func testInfixOperator1(x: Int) {
  x#^INFIX_INT_1^#
}
// INFIX_INT: Begin completions
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  ... {#Int#}[#Range<Int>#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  &+ {#Int#}[#Int#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#Int#}[#Int#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  << {#Int#}[#Int#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  < {#Int#}[#Bool#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#Int#}[#Bool#]
// INFIX_INT-DAG-NOT: &&
// INFIX_INT-DAG-NOT: +=
// INFIX_INT: End completions
func testInfixOperator2(var x: Int) {
  x#^INFIX_INT_2^#
}
// INFIX_LVALUE_INT: Begin completions
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  ... {#Int#}[#Range<Int>#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  &+ {#Int#}[#Int#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#Int#}[#Int#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  << {#Int#}[#Int#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  < {#Int#}[#Bool#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#Int#}[#Bool#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  += {#Int#}[#Void#]
// INFIX_LVALUE_INT-NOT: &&
// INFIX_LVALUE_INT: End completions

func testInfixOperator3(x: String) {
  x#^INFIX_STRING_1^#
}
// INFIX_STRING: Begin completions
// INFIX_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#String#}[#String#]
// INFIX_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#String#}[#Bool#]
// INFIX_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  < {#String#}[#Bool#]
// INFIX_STRING-NOT: +=
// INFIX_STRING-NOT: <<
// INFIX_STRING: End completions

func testInfixOperator4(x: String) {
  x == ""#^INFIX_EXT_STRING_1^#
}
// INFIX_EXT_STRING: Begin completions
// INFIX_EXT_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#String#}[#String#]
// INFIX_EXT_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#Bool#}[#Bool#]
// INFIX_EXT_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  || {#Bool#}[#Bool#]
// INFIX_EXT_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  && {#Bool#}[#Bool#]
// INFIX_EXT_STRING: End completions
