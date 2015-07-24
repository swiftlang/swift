// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_1 > %t.members1.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_1 < %t.members1.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members1.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_2B > %t.members2a.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_2 < %t.members2a.txt
// FIXME: _prext_filter?
// RUN-disabled: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members2a.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_2B > %t.members2b.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_2 < %t.members2b.txt
// FIXME: _prext_filter?
// RUN-disabled: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members2b.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_3 > %t.members3.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_3 < %t.members3.txt
// FIXME: _prext_filter?
// RUN-disabled: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members3.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_4 > %t.members4.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_4 < %t.members4.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members4.txt

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
// PRIVATE_NOMINAL_MEMBERS_2-DAG: Decl[InstanceMethod]/Super: map({#({{.*}}): (Self.Generator.Element) -> T##(Self.Generator.Element) -> T#})[#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_2-DAG-NOT: Decl{{.*}}: last
// PRIVATE_NOMINAL_MEMBERS_2: End completions

func protocolExtCollection2<C : CollectionType where C.Index : BidirectionalIndexType>(a: C) {
  a.#^PRIVATE_NOMINAL_MEMBERS_3^#
}

// PRIVATE_NOMINAL_MEMBERS_3: Begin completions
// PRIVATE_NOMINAL_MEMBERS_3-DAG: Decl[InstanceMethod]/Super: map({#({{.*}}): (Self.Generator.Element) -> T##(Self.Generator.Element) -> T#})[#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3-DAG: Decl[InstanceVar]/Super:            last[#Self.Generator.Element?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3-DAG-NOT: Decl{{.*}}:         indexOf({#({{.*}}): Self.Generator.Element
// PRIVATE_NOMINAL_MEMBERS_3-DAG: Decl[InstanceMethod]/Super:         indexOf({#({{.*}}): (Self.Generator.Element) -> Bool##(Self.Generator.Element) -> Bool#})[#Self.Index?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_3: End completions

func protocolExtArray<T : Equatable>(a: [T]) {
  a.#^PRIVATE_NOMINAL_MEMBERS_4^#
}
// PRIVATE_NOMINAL_MEMBERS_4: Begin completions
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super: map({#({{.*}}): (Self.Generator.Element) -> T##(Self.Generator.Element) -> T#})[#[T]#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceVar]/Super:            last[#Self.Generator.Element?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         indexOf({#({{.*}}): Self.Generator.Element#})[#Self.Index?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4-DAG: Decl[InstanceMethod]/Super:         indexOf({#({{.*}}): (Self.Generator.Element) -> Bool##(Self.Generator.Element) -> Bool#})[#Self.Index?#]{{; name=.+}}
// PRIVATE_NOMINAL_MEMBERS_4: End completions
