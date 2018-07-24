// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 > %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_1 > %t.members1.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_1 < %t.members1.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members1.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_2A > %t.members2a.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_2A < %t.members2a.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE_PRIVATE_NOMINAL_MEMBERS_2A < %t.members2a.txt
// FIXME: filter?
// RUN-disabled: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members2a.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_2B > %t.members2b.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_2B < %t.members2b.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE_PRIVATE_NOMINAL_MEMBERS_2B < %t.members2b.txt
// FIXME: filter?
// RUN-disabled: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members2b.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_3 > %t.members3.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_3 < %t.members3.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE_PRIVATE_NOMINAL_MEMBERS_3 < %t.members3.txt
// FIXME: filter?
// RUN-disabled: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members3.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_4 > %t.members4.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_4 < %t.members4.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members4.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_5 > %t.members5.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_5 < %t.members5.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members5.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_6 > %t.members6.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_6 < %t.members6.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members6.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_7 > %t.members7.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_7 < %t.members7.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members7.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_8 > %t.members8.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_8 < %t.members8.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members8.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_9 > %t.members9.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_9 < %t.members9.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members9.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_10 > %t.members10.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_10 < %t.members10.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members10.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURNS_ANY_SEQUENCE | %FileCheck %s -check-prefix=RETURNS_ANY_SEQUENCE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_INT_1 | %FileCheck %s -check-prefix=POSTFIX_RVALUE_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_INT_2 | %FileCheck %s -check-prefix=POSTFIX_LVALUE_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_OPTIONAL_1 | %FileCheck %s -check-prefix=POSTFIX_OPTIONAL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INFIX_INT_1 > %t
// RUN: %FileCheck %s -check-prefix=INFIX_INT < %t
// RUN: %FileCheck %s -check-prefix=NEGATIVE_INFIX_INT < %t

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INFIX_INT_2 | %FileCheck %s -check-prefix=INFIX_LVALUE_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INFIX_STRING_1 | %FileCheck %s -check-prefix=INFIX_STRING
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INFIX_EXT_STRING_1 | %FileCheck %s -check-prefix=INFIX_EXT_STRING

// NO_STDLIB_PRIVATE: Begin completions
// NO_STDLIB_PRIVATE: End completions

#^PLAIN_TOP_LEVEL_1^#

// PLAIN_TOP_LEVEL: Begin completions
// PLAIN_TOP_LEVEL-DAG: Decl[Struct]/OtherModule[Swift]: Array[#Array#]{{; name=.+$}}
// PLAIN_TOP_LEVEL: End completions

func privateNominalMembers(_ a: String) {
  a.#^PRIVATE_NOMINAL_MEMBERS_1^#
}

// PRIVATE_NOMINAL_MEMBERS_1: Begin completions

// FIXME: we should show the qualified String.Index type.
// rdar://problem/20788802
// PRIVATE_NOMINAL_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal: startIndex[#String.Index#]{{; name=.+$}}
// PRIVATE_NOMINAL_MEMBERS_1: End completions

func protocolExtCollection1a<C : Collection>(_ a: C) {
  a.#^PRIVATE_NOMINAL_MEMBERS_2A^#
}

// PRIVATE_NOMINAL_MEMBERS_2A: Begin completions
// PRIVATE_NOMINAL_MEMBERS_2A-DAG: map({#(transform): (C.Element) throws -> T##(C.Element) throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_2A: End completions
// NEGATIVE_PRIVATE_NOMINAL_MEMBERS_2A-NOT: Decl{{.*}}: index({#before: Comparable#})

func protocolExtCollection1b(_ a: Collection) {
  a.#^PRIVATE_NOMINAL_MEMBERS_2B^#
}

// PRIVATE_NOMINAL_MEMBERS_2B: Begin completions
// PRIVATE_NOMINAL_MEMBERS_2B-DAG: map({#(transform): (Collection.Element) throws -> T##(Collection.Element) throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_2B: End completions
// NEGATIVE_PRIVATE_NOMINAL_MEMBERS_2B-NOT: Decl{{.*}}: index({#before: Comparable#})

func protocolExtCollection2<C : Collection where C.Index : BidirectionalIndex>(_ a: C) {
  a.#^PRIVATE_NOMINAL_MEMBERS_3^#
}

// PRIVATE_NOMINAL_MEMBERS_3: Begin completions
// PRIVATE_NOMINAL_MEMBERS_3-DAG: Decl[InstanceMethod]/Super:         map({#(transform): (C.Element) throws -> T##(C.Element) throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3-DAG: Decl[InstanceVar]/Super:            lazy[#LazySequence<Collection>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3-DAG: firstIndex({#where: (C.Element) throws -> Bool##(C.Element) throws -> Bool#})[' rethrows'][#Comparable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3: End completions
// NEGATIVE_PRIVATE_NOMINAL_MEMBERS_3-NOT: Decl{{.*}}:         firstIndex({#({{.*}}): Self.Iterator.Element

func protocolExtArray<T : Equatable>(_ a: [T]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_4^#
}
// PRIVATE_NOMINAL_MEMBERS_4: Begin completions
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         map({#(transform): (Equatable) throws -> T##(Equatable) throws -> T#})[' rethrows'][#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceVar]/Super:            last[#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         firstIndex({#of: Equatable#})[#Int?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         firstIndex({#where: (Equatable) throws -> Bool##(Equatable) throws -> Bool#})[' rethrows'][#Int?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4: End completions

func testArchetypeReplacement1<FOO : Equatable>(_ a: [FOO]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_5^#
}

// PRIVATE_NOMINAL_MEMBERS_5: Begin completions
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): Equatable#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/CurrNominal:   insert({#(newElement): Equatable#}, {#at: Int#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceVar]/Super:            isEmpty[#Bool#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceVar]/Super:            first[#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         dropFirst({#(k): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         dropLast({#(k): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         prefix({#(maxLength): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_5-DAG: Decl[InstanceMethod]/Super:         suffix({#(maxLength): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}


func testArchetypeReplacement2<BAR : Equatable>(_ a: [BAR]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_6^#
}

// PRIVATE_NOMINAL_MEMBERS_6: Begin completions
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): Equatable#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/CurrNominal:   insert({#(newElement): Equatable#}, {#at: Int#})[#Void#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         dropFirst()[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         dropLast()[#ArraySlice<Equatable>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         enumerated()[#EnumeratedSequence<[Equatable]>#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         min({#by: (Equatable, Equatable) throws -> Bool##(Equatable, Equatable) throws -> Bool#})[' rethrows'][#Equatable?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         max({#by: (Equatable, Equatable) throws -> Bool##(Equatable, Equatable) throws -> Bool#})[' rethrows'][#Equatable?#]{{; name=.+}}
// FIXME: The following should include 'partialResult' as local parameter name: "(nextPartialResult): (_ partialResult: Result, Equatable)"
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         reduce({#(initialResult): Result#}, {#(nextPartialResult): (Result, Equatable) throws -> Result##(Result, Equatable) throws -> Result#})[' rethrows'][#Result#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_6-DAG: Decl[InstanceMethod]/Super:         dropFirst({#(k): Int#})[#ArraySlice<Equatable>#]{{; name=.+}}
// FIXME: restore Decl[InstanceMethod]/Super:         flatMap({#(transform): (Equatable) throws -> Sequence##(Equatable) throws -> Sequence#})[' rethrows'][#[IteratorProtocol.Element]#]{{; name=.+}}

func testArchetypeReplacement3 (_ a : [Int]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_7^#
}

// PRIVATE_NOMINAL_MEMBERS_7: Begin completions
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/CurrNominal:   append({#(newElement): Int#})[#Void#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         removeLast()[#Int#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceVar]/Super:            first[#Int?#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         map({#(transform): (Int) throws -> T##(Int) throws -> T#})[' rethrows'][#[T]#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         dropLast({#(k): Int#})[#ArraySlice<Int>#]
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         elementsEqual({#(other): Sequence#}, {#by: (Int, Sequence.Element) throws -> Bool##(Int, Sequence.Element) throws -> Bool#})[' rethrows'][#Bool#]; name=elementsEqual(other: Sequence, by: (Int, Sequence.Element) throws -> Bool) rethrows
// PRIVATE_NOMINAL_MEMBERS_7-DAG: Decl[InstanceMethod]/Super:         elementsEqual({#(other): Sequence#})[#Bool#]; name=elementsEqual(other: Sequence)


protocol P2 {
  associatedtype MyElement
}

extension P2 {
  func foo(_ x: MyElement) {}
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
  func foo<T: P1 & P2>(_ t : T) {}
}

func testArchetypeReplacement4(_ a : MyClass1) {
  a.#^PRIVATE_NOMINAL_MEMBERS_8^#
}
// PRIVATE_NOMINAL_MEMBERS_8: Begin completions
// PRIVATE_NOMINAL_MEMBERS_8-DAG: Decl[InstanceMethod]/Super: foo({#(x): MyInt#})[#Void#]{{; name=.+}}

func testArchetypeReplacement5(_ a : MyClass2) {
  a.#^PRIVATE_NOMINAL_MEMBERS_9^#
}

// PRIVATE_NOMINAL_MEMBERS_9: Begin completions
// PRIVATE_NOMINAL_MEMBERS_9-DAG: Decl[InstanceMethod]/Super: foo({#(x): Int#})[#Void#]{{; name=.+}}

func testArchetypeReplacement6() {
  var a = MyClass3()
  a.#^PRIVATE_NOMINAL_MEMBERS_10^#
}

// PRIVATE_NOMINAL_MEMBERS_10: Begin completions
// PRIVATE_NOMINAL_MEMBERS_10-DAG: Decl[InstanceMethod]/CurrNominal:   foo({#(t): P1 & P2#})[#Void#]{{; name=.+}}

// rdar://problem/22334700
struct Test1000 : Sequence {
  func #^RETURNS_ANY_SEQUENCE^#
}
// RETURNS_ANY_SEQUENCE: Decl[InstanceMethod]/Super:         dropFirst(_ k: Int)

func testPostfixOperator1(_ x: Int) {
  x#^POSTFIX_INT_1^#
}
// POSTFIX_RVALUE_INT-NOT: ++
// POSTFIX_RVALUE_INT-NOT: --

func testPostfixOperator2(_ x: inout Int) {
  x#^POSTFIX_INT_2^#
}
// POSTFIX_LVALUE_INT-NOT: Decl[PostfixOperatorFunction]/OtherModule[Swift]: ++[#Int#]; name=
// POSTFIX_LVALUE_INT-NOT: Decl[PostfixOperatorFunction]/OtherModule[Swift]: --[#Int#]; name=

func testPostfixOperator3(_ x: MyInt??) {
  x#^POSTFIX_OPTIONAL_1^#
}
// POSTFIX_OPTIONAL: BuiltinOperator/None: ![#MyInt?#]; name=!

func testInfixOperator1(_ x: Int) {
  x#^INFIX_INT_1^#
}
// INFIX_INT: Begin completions
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  ... {#Int#}[#ClosedRange<Int>#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  &+ {#Int#}[#Int#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#Int#}[#Int#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  &<< {#Int#}[#Int#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  < {#Int#}[#Bool#]
// INFIX_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#Int#}[#Bool#]
// INFIX_INT: End completions
// NEGATIVE_INFIX_INT-NOT: &&
// NEGATIVE_INFIX_INT-NOT: +=
func testInfixOperator2(_ x: inout Int) {
  x#^INFIX_INT_2^#
}
// INFIX_LVALUE_INT: Begin completions
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  ... {#Int#}[#ClosedRange<Int>#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  &+ {#Int#}[#Int#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#Int#}[#Int#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  &<< {#Int#}[#Int#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  < {#Int#}[#Bool#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#Int#}[#Bool#]
// INFIX_LVALUE_INT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  += {#Int#}[#Void#]
// INFIX_LVALUE_INT-NOT: &&
// INFIX_LVALUE_INT: End completions

func testInfixOperator3(_ x: String) {
  x#^INFIX_STRING_1^#
}
// INFIX_STRING: Begin completions
// INFIX_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#String#}[#String#]
// INFIX_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#String#}[#Bool#]
// INFIX_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  < {#String#}[#Bool#]
// INFIX_STRING-NOT: +=
// INFIX_STRING-NOT: <<
// INFIX_STRING: End completions

func testInfixOperator4(_ x: String) {
  x == ""#^INFIX_EXT_STRING_1^#
}
// INFIX_EXT_STRING: Begin completions
// INFIX_EXT_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#String#}[#String#]
// INFIX_EXT_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#Bool#}[#Bool#]
// INFIX_EXT_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  || {#Bool#}[#Bool#]
// INFIX_EXT_STRING-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  && {#Bool#}[#Bool#]
// INFIX_EXT_STRING: End completions
