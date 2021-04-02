// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

class A1<T1, T2, T3> {}

class A2<T4, T5> {}

protocol P1 {}

extension A1 where #^GP1?check=A1^#{}

extension A1 where T1 : P1, #^GP2?check=A1^# {}

extension A1 where T1 : P1, #^GP3?check=A1^#

extension A1 where T1 : #^GP4?check=TYPE1^#

extension A1 where T1 : P1, T2 : #^GP5?check=TYPE1^#

extension A1 where T1.#^GP6?check=EMPTY^# {}

// A1: Begin completions
// A1-DAG: Decl[GenericTypeParam]/Local:       T1[#T1#]; name=T1
// A1-DAG: Decl[GenericTypeParam]/Local:       T2[#T2#]; name=T2
// A1-DAG: Decl[GenericTypeParam]/Local:       T3[#T3#]; name=T3
// A1-DAG: Decl[Class]/Local:                  A1[#A1#]; name=A1
// A1-NOT: T4
// A1-NOT: T5
// A1-NOT: Self

// TYPE1: Begin completions
// TYPE1-DAG: Decl[Protocol]/CurrModule:          P1[#P1#]; name=P1
// TYPE1-DAG: Decl[Class]/CurrModule:             A1[#A1#]; name=A1
// TYPE1-DAG: Decl[Class]/CurrModule:             A2[#A2#]; name=A2
// TYPE1-NOT: T1
// TYPE1-NOT: T2
// TYPE1-NOT: T3
// TYPE1-NOT: T4
// TYPE1-NOT: T5
// TYPE1-NOT: Self

// EMPTY: Begin completions, 1 items
// EMPTY-DAG: Keyword/None: Type[#T1.Type#]; name=Type
// EMPTY: End completions

protocol A {associatedtype E}
protocol B {associatedtype E}

protocol C {associatedtype E}
protocol D: C {associatedtype E}

func ab<T: A & B>(_ arg: T) where T.#^FUNC_ASSOC_NODUP_1?check=GEN_T_ASSOC_E^#

func ab<T: D>(_ arg: T) where T.#^FUNC_ASSOC_NODUP_2?check=GEN_T_ASSOC_E^#

// GEN_T_ASSOC_E: Begin completions, 2 items
// GEN_T_ASSOC_E-NEXT: Decl[AssociatedType]/{{Super|CurrNominal}}: E; name=E
// GEN_T_ASSOC_E-NEXT: Keyword/None:               Type[#T.Type#];
// GEN_T_ASSOC_E: End completions

protocol Assoc {
  associatedtype Q
}

func f1<T>(_: T) where #^FUNC_1?check=GEN_T^# {}
// GEN_T: Decl[GenericTypeParam]/Local: T[#T#]; name=T
func f2<T>(_: T) where T.#^FUNC_2?check=GEN_T_DOT^# {}
// GEN_T_DOT: Begin completions
// GEN_T_DOT-DAG: Keyword/None:                       Type[#T.Type#];
// GEN_T_DOT-NOT: Keyword/CurrNominal:                self[#T#];
// GEN_T_DOT: End completions
func f2b<T: Assoc>(_: T) where T.#^FUNC_2_ASSOC?check=GEN_T_ASSOC_DOT^# {}
// GEN_T_ASSOC_DOT: Begin completions
// GEN_T_ASSOC_DOT-DAG: Decl[AssociatedType]/{{Super|CurrNominal}}: Q;
// GEN_T_ASSOC_DOT-DAG: Keyword/None:                       Type[#T.Type#];
// GEN_T_ASSOC_DOT-NOT: Keyword/CurrNominal:                self[#T#];
// GEN_T_ASSOC_DOT: End completions
func f3<T>(_: T) where T == #^FUNC_3?check=GEN_T^# {}
func f3<T>(_: T) where T == T.#^FUNC_4?check=GEN_T_DOT^# {}
struct S1 {
  func f1<T>(_: T) where #^FUNC_5?check=GEN_T_S1^# {}
  func f2<T>(_: T) where T.#^FUNC_6?check=GEN_T_DOT^# {}
  subscript<T>(x: T) -> T where #^SUBSCRIPT_1?check=GEN_T_S1^# { return x }
  subscript<T>(x: T) -> T where T.#^SUBSCRIPT_2?check=GEN_T_DOT^# { return x }
  init<T>(_: T) where #^INIT_1?check=GEN_T_S1^# {}
  init<T>(_: T) where T.#^INIT_2?check=GEN_T_DOT^# {}
  typealias TA1<T> = A1<T, T, T> where #^ALIAS_1?check=GEN_T_S1^#
  typealias TA2<T> = A1<T, T, T> where T.#^ALIAS_2?check=GEN_T_DOT^#
}
// GEN_T_S1: Begin completions, 3 items
// GEN_T_S1-DAG: Decl[GenericTypeParam]/Local: T[#T#];
// GEN_T_S1-DAG: Decl[Struct]/Local:           S1[#S1#];
// GEN_T_S1-DAG: Keyword[Self]/CurrNominal:    Self[#S1#];
// GEN_T_S1: End completions

struct S2<T> where #^STRUCT_1?check=GEN_T_NOMINAL^# {}
struct S3<T> where T.#^STRUCT_2?check=GEN_T_DOT^# {}
struct S4<T> where T == #^STRUCT_3?check=ANYTYPE^# {}
struct S5<T> where T == T.#^STRUCT_4?check=GEN_T_DOT^# {}
class C1<T> where #^CLASS_1?check=GEN_T_NOMINAL^# {}
class C2<T> where T.#^CLASS_2?check=GEN_T_DOT^# {}
enum E1<T> where #^ENUM_1?check=GEN_T_NOMINAL^# {}
enum E2<T> where T.#^ENUM_2?check=GEN_T_DOT^# {}
// GEN_T_NOMINAL: Begin completions, 1 items
// GEN_T_NOMINAL: Decl[GenericTypeParam]/Local: T[#T#]; name=T
// GEN_T_NOMINAL: End completions

// ANYTYPE: Begin completions
// ANYTYPE-DAG: Decl[GenericTypeParam]/Local: T[#T#];
// ANYTYPE-DAG: Decl[Class]/CurrModule: A1[#A1#];
// ANYTYPE-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Int[#Int#];
// ANYTYPE: End completions

protocol P2 {
  associatedtype T where #^ASSOC_1?check=P2^#
  associatedtype U: Assoc where U.#^ASSOC_2?check=U_DOT^#
}

// P2: Begin completions, 4 items
// P2-DAG: Decl[GenericTypeParam]/Local:               Self[#Self#];
// P2-DAG: Decl[AssociatedType]/{{Super|CurrNominal}}: T;
// P2-DAG: Decl[AssociatedType]/{{Super|CurrNominal}}: U;
// P2-DAG: Decl[Protocol]/Local:                       P2[#P2#]
// P2: End completions

// U_DOT: Begin completions, 2 items
// U_DOT-DAG: Keyword/None:                       Type[#Self.U.Type#];
// U_DOT-DAG: Decl[AssociatedType]/CurrNominal:   Q;
// U_DOT: End completions

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
// PROTOCOL: End completions

extension P3 where #^PROTOCOL_EXT?check=PROTOCOL^# {
  // Same as PROTOCOL
}

protocol P4 where Self.#^PROTOCOL_SELF^# {
  associatedtype T: Assoc
  typealias U = T.Q
  typealias IntAlias = Int
}
// PROTOCOL_SELF: Begin completions
// PROTOCOL_SELF-DAG: Decl[AssociatedType]/CurrNominal:   T;
// PROTOCOL_SELF-DAG: Decl[TypeAlias]/CurrNominal:        U[#Self.T.Q#];
// PROTOCOL_SELF-DAG: Decl[TypeAlias]/CurrNominal:        IntAlias[#Int#];
// PROTOCOL_SELF-DAG: Keyword/None:                       Type[#Self.Type#];
// PROTOCOL_SELF: End completions

struct TA1<T: Assoc> where #^NOMINAL_TYPEALIAS^# {
  typealias U = T.Q
}
// NOMINAL_TYPEALIAS: Begin completions, 1 items
// NOMINAL_TYPEALIAS-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS: End completions
extension TA1 where #^NOMINAL_TYPEALIAS_EXT^# { }
// NOMINAL_TYPEALIAS_EXT: Begin completions, 4 items
// NOMINAL_TYPEALIAS_EXT-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_EXT-DAG: Decl[TypeAlias]/CurrNominal:        U[#T.Q#];
// NOMINAL_TYPEALIAS_EXT-DAG: Decl[Struct]/Local:                 TA1[#TA1#];
// NOMINAL_TYPEALIAS_EXT-DAG: Keyword[Self]/CurrNominal:          Self[#TA1<T>#];
// NOMINAL_TYPEALIAS_EXT: End completions

struct TA2<T: Assoc> {
  struct Inner1<U> where #^NOMINAL_TYPEALIAS_NESTED1^# {
    typealias X1 = T
    typealias X2 = T.Q
  }
// NOMINAL_TYPEALIAS_NESTED1: Begin completions, 2 items
// NOMINAL_TYPEALIAS_NESTED1-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_NESTED1-DAG: Decl[GenericTypeParam]/Local:       U[#U#];
// NOMINAL_TYPEALIAS_NESTED1: End completions
  struct Inner2 where #^NOMINAL_TYPEALIAS_NESTED2^# {
    typealias X1 = T
    typealias X2 = T.Q
  }
// NOMINAL_TYPEALIAS_NESTED2: Begin completions, 1 items
// NOMINAL_TYPEALIAS_NESTED2-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_NESTED2: End completions
}
extension TA2.Inner1 where #^NOMINAL_TYPEALIAS_NESTED1_EXT^# {}
// NOMINAL_TYPEALIAS_NESTED1_EXT: Begin completions, 6 items
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[GenericTypeParam]/Local:       U[#U#];
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[TypeAlias]/CurrNominal:        X1[#T#];
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[TypeAlias]/CurrNominal:        X2[#T.Q#];
// FIXME : We shouldn't be suggesting Inner1 because it's not fully-qualified
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Decl[Struct]/Local:                 Inner1[#TA2.Inner1#];
// NOMINAL_TYPEALIAS_NESTED1_EXT-DAG: Keyword[Self]/CurrNominal:          Self[#TA2<T>.Inner1<U>#];
// NOMINAL_TYPEALIAS_NESTED1_EXT: End completions
extension TA2.Inner2 where #^NOMINAL_TYPEALIAS_NESTED2_EXT^# {}
// NOMINAL_TYPEALIAS_NESTED2_EXT: Begin completions, 5 items
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Decl[GenericTypeParam]/Local:       T[#T#];
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Decl[TypeAlias]/CurrNominal:        X1[#T#];
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Decl[TypeAlias]/CurrNominal:        X2[#T.Q#];
// FIXME : We shouldn't be suggesting Inner2 because it's not fully-qualified
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Decl[Struct]/Local:                 Inner2[#TA2.Inner2#];
// NOMINAL_TYPEALIAS_NESTED2_EXT-DAG: Keyword[Self]/CurrNominal:          Self[#TA2<T>.Inner2#];
// NOMINAL_TYPEALIAS_NESTED2_EXT: End completions

protocol WithAssoc {
  associatedtype T: Assoc
}
extension WithAssoc where T.#^EXT_ASSOC_MEMBER_1?check=EXT_ASSOC_MEMBER^# 
// EXT_ASSOC_MEMBER: Begin completions, 2 items
// EXT_ASSOC_MEMBER-DAG: Decl[AssociatedType]/CurrNominal:   Q;
// EXT_ASSOC_MEMBER-DAG: Keyword/None:                       Type[#Self.T.Type#];
// EXT_ASSOC_MEMBER: End completions

extension WithAssoc where Int == T.#^EXT_ASSOC_MEMBER_2?check=EXT_ASSOC_MEMBER^# 
// Same as EXT_ASSOC_MEMBER

extension WithAssoc where Int == #^EXT_SECONDTYPE^# 
// EXT_SECONDTYPE: Begin completions
// EXT_SECONDTYPE-DAG: Decl[AssociatedType]/CurrNominal:   T;
// EXT_SECONDTYPE: End completions
