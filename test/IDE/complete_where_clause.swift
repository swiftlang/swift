// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP1 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP2 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP3 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP4 | %FileCheck %s -check-prefix=TYPE1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP5 | %FileCheck %s -check-prefix=TYPE1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP6 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_ASSOC_NODUP_1 | %FileCheck %s -check-prefix=GEN_T_ASSOC_E
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_ASSOC_NODUP_2 | %FileCheck %s -check-prefix=GEN_T_ASSOC_E
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_1 | %FileCheck %s -check-prefix=GEN_T
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_2_ASSOC | %FileCheck %s -check-prefix=GEN_T_ASSOC_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_3 | %FileCheck %s -check-prefix=GEN_T
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_4 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_5 | %FileCheck %s -check-prefix=GEN_T
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_6 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_1 | %FileCheck %s -check-prefix=GEN_T
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_1 | %FileCheck %s -check-prefix=GEN_T
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ALIAS_1 | %FileCheck %s -check-prefix=GEN_T
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ALIAS_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_1 | %FileCheck %s -check-prefix=GEN_T_NOMINAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_3 | %FileCheck %s -check-prefix=GEN_T_NOMINAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_4 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_1 | %FileCheck %s -check-prefix=GEN_T_NOMINAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_1 | %FileCheck %s -check-prefix=GEN_T_NOMINAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSOC_1 | %FileCheck %s -check-prefix=P2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSOC_2 | %FileCheck %s -check-prefix=U_DOT

class A1<T1, T2, T3> {}

class A2<T4, T5> {}

protocol P1 {}

extension A1 where #^GP1^#{}

extension A1 where T1 : P1, #^GP2^# {}

extension A1 where T1 : P1, #^GP3^#

extension A1 where T1 : #^GP4^#

extension A1 where T1 : P1, T2 : #^GP5^#

extension A1 where T1.#^GP6^# {}

// A1: Begin completions
// A1-DAG: Decl[GenericTypeParam]/Local:       T1[#T1#]; name=T1
// A1-DAG: Decl[GenericTypeParam]/Local:       T2[#T2#]; name=T2
// A1-DAG: Decl[GenericTypeParam]/Local:       T3[#T3#]; name=T3
// A1-NOT: T4
// A1-NOT: T5

// TYPE1: Begin completions
// TYPE1-DAG: Decl[Protocol]/CurrModule:          P1[#P1#]; name=P1
// TYPE1-DAG: Decl[Class]/CurrModule:             A1[#A1#]; name=A1
// TYPE1-DAG: Decl[Class]/CurrModule:             A2[#A2#]; name=A2
// TYPE1-NOT: T1
// TYPE1-NOT: T2
// TYPE1-NOT: T3
// TYPE1-NOT: T4
// TYPE1-NOT: T5

protocol A {associatedtype E}
protocol B {associatedtype E}

protocol C {associatedtype E}
protocol D: C {associatedtype E}

func ab<T: A & B>(_ arg: T) where T.#^FUNC_ASSOC_NODUP_1^#

func ab<T: D>(_ arg: T) where T.#^FUNC_ASSOC_NODUP_2^#

// GEN_T_ASSOC_E: Begin completions, 2 items
// GEN_T_ASSOC_E-NEXT: Decl[AssociatedType]/Super: E; name=E
// GEN_T_ASSOC_E-NOT:  Decl[AssociatedType]/Super: E; name=E
// GEN_T_ASSOC_E-NEXT: Keyword/None:               Type[#T.Type#];
// GEN_T_ASSOC_E: End completions

protocol Assoc {
  associatedtype Q
}

func f1<T>(_: T) where #^FUNC_1^# {}
// GEN_T: Decl[GenericTypeParam]/Local: T[#T#]; name=T
func f2<T>(_: T) where T.#^FUNC_2^# {}
// GEN_T_DOT: Begin completions
// GEN_T_DOT-DAG: Keyword/None:                       Type[#T.Type#];
// GEN_T_DOT-NOT: Keyword/CurrNominal:                self[#T#];
// GEN_T_DOT: End completions
func f2b<T: Assoc>(_: T) where T.#^FUNC_2_ASSOC^# {}
// GEN_T_ASSOC_DOT: Begin completions
// GEN_T_ASSOC_DOT-DAG: Decl[AssociatedType]/Super:         Q;
// GEN_T_ASSOC_DOT-DAG: Keyword/None:                       Type[#T.Type#];
// GEN_T_ASSOC_DOT-NOT: Keyword/CurrNominal:                self[#T#];
// GEN_T_ASSOC_DOT: End completions
func f3<T>(_: T) where T == #^FUNC_3^# {}
func f3<T>(_: T) where T == T.#^FUNC_4^# {}
struct S1 {
  func f1<T>(_: T) where #^FUNC_5^# {}
  func f2<T>(_: T) where T.#^FUNC_6^# {}
  subscript<T>(x: T) -> T where #^SUBSCRIPT_1^# { return x }
  subscript<T>(x: T) -> T where T.#^SUBSCRIPT_2^# { return x }
  init<T>(_: T) where #^INIT_1^# {}
  init<T>(_: T) where T.#^INIT_2^# {}
  typealias TA1<T> = A1<T, T, T> where #^ALIAS_1^#
  typealias TA2<T> = A1<T, T, T> where T.#^ALIAS_2^#
}

struct S2<T> where #^STRUCT_1^# {}
struct S3<T> where T.#^STRUCT_2^# {}
struct S4<T> where T == #^STRUCT_3^# {}
struct S5<T> where T == T.#^STRUCT_4^# {}
class C1<T> where #^CLASS_1^# {}
class C2<T> where T.#^CLASS_2^# {}
enum E1<T> where #^ENUM_1^# {}
enum E2<T> where T.#^ENUM_2^# {}
// GEN_T_NOMINAL: Decl[GenericTypeParam]/CurrNominal: T[#T#]; name=T

protocol P2 {
  associatedtype T where #^ASSOC_1^#
  associatedtype U: Assoc where U.#^ASSOC_2^#
}

// P2: Begin completions
// P2-DAG: Decl[GenericTypeParam]/Super: Self[#Self#];
// P2-DAG: Decl[AssociatedType]/Super:   T;
// P2-DAG: Decl[AssociatedType]/Super:   U;
// P2: End completions

// U_DOT: Begin completions
// FIXME: Should complete Q from Assoc.
// U_DOT-DAG: Keyword/None:                       Type[#Self.U.Type#];
// U_DOT: End completions
