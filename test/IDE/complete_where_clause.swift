// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP1 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP2 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP3 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP4 | %FileCheck %s -check-prefix=TYPE1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP5 | %FileCheck %s -check-prefix=TYPE1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP6 | %FileCheck %s -check-prefix=EMPTY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_ASSOC_NODUP_1 | %FileCheck %s -check-prefix=GEN_T_ASSOC_E
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_ASSOC_NODUP_2 | %FileCheck %s -check-prefix=GEN_T_ASSOC_E
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_1 | %FileCheck %s -check-prefix=GEN_T
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_2_ASSOC | %FileCheck %s -check-prefix=GEN_T_ASSOC_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_3 | %FileCheck %s -check-prefix=GEN_T
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_4 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_5 | %FileCheck %s -check-prefix=GEN_T_S1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_6 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_1 | %FileCheck %s -check-prefix=GEN_T_S1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_1 | %FileCheck %s -check-prefix=GEN_T_S1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ALIAS_1 | %FileCheck %s -check-prefix=GEN_T_S1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ALIAS_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_1 | %FileCheck %s -check-prefix=GEN_T_NOMINAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_3 | %FileCheck %s -check-prefix=ANYTYPE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_4 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_1 | %FileCheck %s -check-prefix=GEN_T_NOMINAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_1 | %FileCheck %s -check-prefix=GEN_T_NOMINAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_2 | %FileCheck %s -check-prefix=GEN_T_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSOC_1 | %FileCheck %s -check-prefix=P2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSOC_2 | %FileCheck %s -check-prefix=U_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL | %FileCheck %s -check-prefix=PROTOCOL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT | %FileCheck %s -check-prefix=PROTOCOL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_SELF | %FileCheck %s -check-prefix=PROTOCOL_SELF
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NOMINAL_TYPEALIAS | %FileCheck %s -check-prefix=NOMINAL_TYPEALIAS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NOMINAL_TYPEALIAS_EXT | %FileCheck %s -check-prefix=NOMINAL_TYPEALIAS_EXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NOMINAL_TYPEALIAS_NESTED1 | %FileCheck %s -check-prefix=NOMINAL_TYPEALIAS_NESTED1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NOMINAL_TYPEALIAS_NESTED2 | %FileCheck %s -check-prefix=NOMINAL_TYPEALIAS_NESTED2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NOMINAL_TYPEALIAS_NESTED1_EXT | %FileCheck %s -check-prefix=NOMINAL_TYPEALIAS_NESTED1_EXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NOMINAL_TYPEALIAS_NESTED2_EXT | %FileCheck %s -check-prefix=NOMINAL_TYPEALIAS_NESTED2_EXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXT_ASSOC_MEMBER_1 | %FileCheck %s -check-prefix=EXT_ASSOC_MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXT_ASSOC_MEMBER_2 | %FileCheck %s -check-prefix=EXT_ASSOC_MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXT_SECONDTYPE | %FileCheck %s -check-prefix=EXT_SECONDTYPE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=WHERE_CLAUSE_WITH_EQUAL | %FileCheck %s -check-prefix=WHERE_CLAUSE_WITH_EQUAL

class A1<T1, T2, T3> {}

class A2<T4, T5> {}

protocol P1 {}

extension A1 where #^GP1^#{}

extension A1 where T1 : P1, #^GP2^# {}

extension A1 where T1 : P1, #^GP3^#

extension A1 where T1 : #^GP4^#

extension A1 where T1 : P1, T2 : #^GP5^#

extension A1 where T1.#^GP6^# {}

// A1-DAG: Decl[GenericTypeParam]/Local:       T1[#T1#]; name=T1
// A1-DAG: Decl[GenericTypeParam]/Local:       T2[#T2#]; name=T2
// A1-DAG: Decl[GenericTypeParam]/Local:       T3[#T3#]; name=T3
// A1-DAG: Decl[Class]/Local:                  A1[#A1<T1, T2, T3>#]; name=A1
// A1-NOT: T4
// A1-NOT: T5
// A1-NOT: Self

// TYPE1-DAG: Decl[Protocol]/CurrModule:          P1[#P1#]; name=P1
// TYPE1-DAG: Decl[Class]/CurrModule:             A1[#A1<T1, T2, T3>#]; name=A1
// TYPE1-DAG: Decl[Class]/CurrModule:             A2[#A2<T4, T5>#]; name=A2
// TYPE1-NOT: T1
// TYPE1-NOT: T2
// TYPE1-NOT: T3
// TYPE1-NOT: T4
// TYPE1-NOT: T5
// TYPE1-NOT: Self

// EMPTY: Begin completions, 1 items
// EMPTY-DAG: Keyword/None: Type[#T1.Type#]; name=Type

protocol A {associatedtype E}
protocol B {associatedtype E}

protocol C {associatedtype E}
protocol D: C {associatedtype E}

func ab<T: A & B>(_ arg: T) where T.#^FUNC_ASSOC_NODUP_1^#

func ab<T: D>(_ arg: T) where T.#^FUNC_ASSOC_NODUP_2^#

// GEN_T_ASSOC_E: Begin completions, 2 items
// GEN_T_ASSOC_E-NEXT: Decl[AssociatedType]/{{Super|CurrNominal}}: E; name=E
// GEN_T_ASSOC_E-NEXT: Keyword/None:               Type[#T.Type#];

protocol Assoc {
  associatedtype Q
}

func f1<T>(_: T) where #^FUNC_1^# {}
// GEN_T: Decl[GenericTypeParam]/Local: T[#T#]; name=T
func f2<T>(_: T) where T.#^FUNC_2^# {}
// GEN_T_DOT-DAG: Keyword/None:                       Type[#T.Type#];
// GEN_T_DOT-NOT: Keyword/CurrNominal:                self[#T#];
func f2b<T: Assoc>(_: T) where T.#^FUNC_2_ASSOC^# {}
// GEN_T_ASSOC_DOT-DAG: Decl[AssociatedType]/{{Super|CurrNominal}}: Q;
// GEN_T_ASSOC_DOT-DAG: Keyword/None:                       Type[#T.Type#];
// GEN_T_ASSOC_DOT-NOT: Keyword/CurrNominal:                self[#T#];
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
// GEN_T_S1: Begin completions, 3 items
// GEN_T_S1-DAG: Decl[GenericTypeParam]/Local: T[#T#];
// GEN_T_S1-DAG: Decl[Struct]/Local:           S1[#S1#];
// GEN_T_S1-DAG: Keyword[Self]/CurrNominal:    Self[#S1#];

struct S2<T> where #^STRUCT_1^# {}
struct S3<T> where T.#^STRUCT_2^# {}
struct S4<T> where T == #^STRUCT_3^# {}
struct S5<T> where T == T.#^STRUCT_4^# {}
class C1<T> where #^CLASS_1^# {}
class C2<T> where T.#^CLASS_2^# {}
enum E1<T> where #^ENUM_1^# {}
enum E2<T> where T.#^ENUM_2^# {}
// GEN_T_NOMINAL: Begin completions, 1 items
// GEN_T_NOMINAL: Decl[GenericTypeParam]/Local: T[#T#]; name=T

// ANYTYPE-DAG: Decl[GenericTypeParam]/Local: T[#T#];
// ANYTYPE-DAG: Decl[Class]/CurrModule: A1[#A1<T1, T2, T3>#];
// ANYTYPE-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Int[#Int#];

protocol P2 {
  associatedtype T where #^ASSOC_1^#
  associatedtype U: Assoc where U.#^ASSOC_2^#
}

// P2: Begin completions, 4 items
// P2-DAG: Decl[GenericTypeParam]/Local:               Self[#Self#];
// P2-DAG: Decl[AssociatedType]/{{Super|CurrNominal}}: T;
// P2-DAG: Decl[AssociatedType]/{{Super|CurrNominal}}: U;
// P2-DAG: Decl[Protocol]/Local:                       P2[#P2#]

// U_DOT: Begin completions, 2 items
// U_DOT-DAG: Keyword/None:                       Type[#Self.U.Type#];
// U_DOT-DAG: Decl[AssociatedType]/CurrNominal:   Q;

protocol P3 where #^PROTOCOL^# {
  associatedtype T: Assoc
  typealias U = T.Q
  typealias IntAlias = Int
}
// PROTOCOL: Begin completions, 4 items
// PROTOCOL-DAG: Decl[GenericTypeParam]/Local:       Self[#Self#];
// PROTOCOL-DAG: Decl[AssociatedType]/CurrNominal:   T;
// PROTOCOL-DAG: Decl[TypeAlias]/CurrNominal:        U[#Self.T.Q#];
// PROTOCOL-DAG: Decl[Protocol]/Local:               P3[#P3#];

extension P3 where #^PROTOCOL_EXT^# {
  // Same as PROTOCOL
}

protocol P4 where Self.#^PROTOCOL_SELF^# {
  associatedtype T: Assoc
  typealias U = T.Q
  typealias IntAlias = Int
}
// PROTOCOL_SELF-DAG: Decl[AssociatedType]/CurrNominal:   T;
// PROTOCOL_SELF-DAG: Decl[TypeAlias]/CurrNominal:        U[#Self.T.Q#];
// PROTOCOL_SELF-DAG: Decl[TypeAlias]/CurrNominal:        IntAlias[#Int#];
// PROTOCOL_SELF-DAG: Keyword/None:                       Type[#Self.Type#];

struct TA1<T: Assoc> where #^NOMINAL_TYPEALIAS^# {
  typealias U = T.Q
}
// NOMINAL_TYPEALIAS: Begin completions, 1 items
// NOMINAL_TYPEALIAS-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
extension TA1 where #^NOMINAL_TYPEALIAS_EXT^# { }
// NOMINAL_TYPEALIAS_EXT: Begin completions, 4 items
// NOMINAL_TYPEALIAS_EXT-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_EXT-DAG: Decl[TypeAlias]/CurrNominal:        U[#T.Q#];
// NOMINAL_TYPEALIAS_EXT-DAG: Decl[Struct]/Local:                 TA1[#TA1<T>#];
// NOMINAL_TYPEALIAS_EXT-DAG: Keyword[Self]/CurrNominal:          Self[#TA1<T>#];

struct TA2<T: Assoc> {
  struct Inner1<U> where #^NOMINAL_TYPEALIAS_NESTED1^# {
    typealias X1 = T
    typealias X2 = T.Q
  }
// NOMINAL_TYPEALIAS_NESTED1: Begin completions, 2 items
// NOMINAL_TYPEALIAS_NESTED1-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_NESTED1-DAG: Decl[GenericTypeParam]/Local:       U[#U#];
  struct Inner2 where #^NOMINAL_TYPEALIAS_NESTED2^# {
    typealias X1 = T
    typealias X2 = T.Q
  }
// NOMINAL_TYPEALIAS_NESTED2: Begin completions, 1 items
// NOMINAL_TYPEALIAS_NESTED2-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
}
extension TA2.Inner1 where #^NOMINAL_TYPEALIAS_NESTED1_EXT^# {}
// NOMINAL_TYPEALIAS_NESTED1_EXT: Begin completions, 5 items
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[GenericTypeParam]/Local:       U[#U#];
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[TypeAlias]/CurrNominal:        X1[#T#];
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[TypeAlias]/CurrNominal:        X2[#T.Q#];
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Keyword[Self]/CurrNominal:          Self[#TA2<T>.Inner1<U>#];
extension TA2.Inner2 where #^NOMINAL_TYPEALIAS_NESTED2_EXT^# {}
// NOMINAL_TYPEALIAS_NESTED2_EXT: Begin completions, 4 items
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Decl[TypeAlias]/CurrNominal:        X1[#T#];
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Decl[TypeAlias]/CurrNominal:        X2[#T.Q#];
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Keyword[Self]/CurrNominal:          Self[#TA2<T>.Inner2#];

protocol WithAssoc {
  associatedtype T: Assoc
}
extension WithAssoc where T.#^EXT_ASSOC_MEMBER_1^# 
// EXT_ASSOC_MEMBER: Begin completions, 2 items
// EXT_ASSOC_MEMBER-DAG: Decl[AssociatedType]/CurrNominal:   Q;
// EXT_ASSOC_MEMBER-DAG: Keyword/None:                       Type[#Self.T.Type#];

extension WithAssoc where Int == T.#^EXT_ASSOC_MEMBER_2^# 
// Same as EXT_ASSOC_MEMBER

extension WithAssoc where Int == #^EXT_SECONDTYPE^# 
// EXT_SECONDTYPE-DAG: Decl[AssociatedType]/CurrNominal:   T;

func foo<K: WithAssoc>(_ key: K.Type) where K.#^WHERE_CLAUSE_WITH_EQUAL^# == S1 {}

// WHERE_CLAUSE_WITH_EQUAL: Begin completions, 2 items
// WHERE_CLAUSE_WITH_EQUAL-DAG: Decl[AssociatedType]/CurrNominal:   T;
// WHERE_CLAUSE_WITH_EQUAL-DAG: Keyword/None:                       Type[#K.Type#];
