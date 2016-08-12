// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INHERIT1 | %FileCheck %s -check-prefix=INHERIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INHERIT2 | %FileCheck %s -check-prefix=INHERIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INHERIT3 | %FileCheck %s -check-prefix=INHERIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INHERIT4 | %FileCheck %s -check-prefix=INHERIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INHERIT5 | %FileCheck %s -check-prefix=INHERIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INHERIT6 | %FileCheck %s -check-prefix=INHERIT

class C1{}
protocol P1{}
struct S1{}

let ValueInt1 = 1
let ValueString2 = ""
func TopLevelFunc() {}

func f1<S : #^INHERIT1^#>(p : S) {}
func f2<S : #^INHERIT2^#

class C2 {
  func f1<S : #^INHERIT3^#>(p : S) {}
  func f2<S : #^INHERIT4^#
}

class C3 {
  func f1<S1: P1, S2 : #^INHERIT5^#>(p : S1) {}
  func f2<S1: P1, S2 : #^INHERIT6^#
}

// INHERIT-DAG: Decl[Class]/CurrModule:             C1[#C1#]{{; name=.+$}}

// FIXME: Exclude struct type. rdar://24110708
// INHERIT-DAG: Decl[Struct]/CurrModule:            S1[#S1#]{{; name=.+$}}
// INHERIT-DAG: Decl[Protocol]/CurrModule:          P1[#P1#]{{; name=.+$}}
// INHERIT-NOT: ValueInt1
// INHERIT-NOT: ValueString2
// INHERIT-NOT: TopLevelFunc
