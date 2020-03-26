protocol It {
  associatedtype Assoc
}

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PARAM_0 | %FileCheck %s -check-prefix=TOP_LEVEL_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_0 | %FileCheck %s -check-prefix=TOP_LEVEL_0

struct S0 {
  subscript(x: #^PARAM_0^#) -> Int { return 0 }
  subscript(x: Int) -> #^RETURN_0^# { }
}
// TOP_LEVEL_0: Keyword/None:                       Any[#Any#];
// TOP_LEVEL_0: Decl[Struct]/CurrModule:            S0[#S0#];
// TOP_LEVEL_0: Decl[Struct]/OtherModule[Swift]:    Int[#Int#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PARAM_1 | %FileCheck %s -check-prefix=MYSTRUCT_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_1 | %FileCheck %s -check-prefix=MYSTRUCT_0
struct S1 {
  struct MyStruct {}
  subscript(x: MyStruct#^PARAM_1^#) -> Int { return 0 }
  subscript(x: MyStruct) -> MyStruct#^RETURN_1^# { }
}
// MYSTRUCT_0: Keyword/None:                       .Type[#MyStruct.Type#];


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PARAM_2 | %FileCheck %s -check-prefix=MYSTRUCT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_2 | %FileCheck %s -check-prefix=MYSTRUCT_1
struct S2 {
  struct MyStruct {}
  subscript(x: MyStruct.#^PARAM_2^#) -> Int { return 0 }
  subscript(x: MyStruct) -> MyStruct.#^RETURN_2^# { }
}
// MYSTRUCT_1: Keyword/None:                       Type[#MyStruct.Type#];
// MYSTRUCT_1-NOT: Keyword/CurrNominal:            self[#MyStruct#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_PARAM_0 | %FileCheck %s -check-prefix=GEN_TOP_LEVEL_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_RETURN_0 | %FileCheck %s -check-prefix=GEN_TOP_LEVEL_0
struct G0<T> {
  subscript(x: #^GEN_PARAM_0^#) -> Int { return 0 }
  subscript(x: T) -> #^GEN_RETURN_0^# { return 0 }
}
// GEN_TOP_LEVEL_0: Keyword/None:                       Any[#Any#];
// GEN_TOP_LEVEL_0: Decl[GenericTypeParam]/Local:       T[#T#]; name=T
// GEN_TOP_LEVEL_0: Decl[Struct]/CurrModule:            S0[#S0#];
// GEN_TOP_LEVEL_0: Decl[Struct]/OtherModule[Swift]:    Int[#Int#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_PARAM_1 | %FileCheck %s -check-prefix=GEN_PARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_RETURN_1 | %FileCheck %s -check-prefix=GEN_PARAM_1
struct G1<T> {
  subscript(x: T#^GEN_PARAM_1^#) -> Int { return 0 }
  subscript(x: T) -> T#^GEN_RETURN_1^# { return 0 }
}
// GEN_PARAM_1: Keyword/None:                       .Type[#T.Type#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_PARAM_2 | %FileCheck %s -check-prefix=GEN_PARAM_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_RETURN_2 | %FileCheck %s -check-prefix=GEN_PARAM_2
struct G2<T> {
  subscript(x: T.#^GEN_PARAM_2^#) -> Int { return 0 }
  subscript(x: T) -> T.#^GEN_RETURN_2^# { return 0 }
}
// GEN_PARAM_2: Keyword/None:                       Type[#T.Type#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_PARAM_3 | %FileCheck %s -check-prefix=GEN_TOP_LEVEL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_RETURN_3 | %FileCheck %s -check-prefix=GEN_TOP_LEVEL_1
struct G3 {
  subscript<T>(x: #^GEN_PARAM_3^#) -> Int { return 0 }
  subscript<T>(x: T) -> #^GEN_RETURN_3^# { return 0 }
}
// GEN_TOP_LEVEL_1: Keyword/None:                       Any[#Any#];
// GEN_TOP_LEVEL_1: Decl[GenericTypeParam]/Local:       T[#T#];
// GEN_TOP_LEVEL_1: Decl[Struct]/CurrModule:            S0[#S0#];
// GEN_TOP_LEVEL_1: Decl[Struct]/OtherModule[Swift]:    Int[#Int#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_PARAM_4 | %FileCheck %s -check-prefix=GEN_PARAM_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_RETURN_4 | %FileCheck %s -check-prefix=GEN_PARAM_4
struct G4 {
  subscript<T>(x: T#^GEN_PARAM_4^#) -> Int { return 0 }
  subscript<T>(x: T) -> T#^GEN_RETURN_4^# { return 0 }
}
// GEN_PARAM_4: Keyword/None:                       .Type[#T.Type#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_PARAM_5 | %FileCheck %s -check-prefix=GEN_PARAM_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_RETURN_5 | %FileCheck %s -check-prefix=GEN_PARAM_5
struct G5 {
  subscript<T>(x: T.#^GEN_PARAM_5^#) -> Int { return 0 }
  subscript<T>(x: T) -> T.#^GEN_RETURN_5^# { return 0 }
}
// GEN_PARAM_5: Keyword/None:                       Type[#T.Type#];
// GEN_PARAM_5-NOT: Keyword/CurrNominal:            self[#T#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_PARAM_6 | %FileCheck %s -check-prefix=GEN_PARAM_6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_RETURN_6 | %FileCheck %s -check-prefix=GEN_PARAM_6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_EXT_PARAM_6 | %FileCheck %s -check-prefix=GEN_PARAM_6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GEN_EXT_RETURN_6 | %FileCheck %s -check-prefix=GEN_PARAM_6
struct G6<T: It> {
  subscript(a x: T.#^GEN_PARAM_6^#) -> Int { return 0 }
  subscript(a x: Int) -> T.#^GEN_RETURN_6^# { return 0 }
}
extension G6 {
  subscript(b x: T.#^GEN_EXT_PARAM_6^#) -> Int { return 0 }
  subscript(b x: Int) -> T.#^GEN_EXT_RETURN_6^# { return 0 }
}
// GEN_PARAM_6-DAG: Decl[AssociatedType]/CurrNominal:   Assoc;
// GEN_PARAM_6-DAG: Keyword/None:                       Type[#T.Type#];

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENPROTO_PARAM_1 | %FileCheck %s -check-prefix=GENPROTO_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENPROTO_RETURN_1 | %FileCheck %s -check-prefix=GENPROTO_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENPROTO_EXT_PARAM_1 | %FileCheck %s -check-prefix=GENPROTO_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENPROTO_EXT_RETURN_1 | %FileCheck %s -check-prefix=GENPROTO_1
protocol GP1 {
  associatedtype I: It
  subscript(a x: I.#^GENPROTO_PARAM_1^#) -> Int
  subscript(ax: Int) -> I.#^GENPROTO_RETURN_1^#
}
extension GP1 {
  subscript(b x: I.#^GENPROTO_EXT_PARAM_1^#) -> Int { return 1 }
  subscript(b x: Int) -> I.#^GENPROTO_EXT_RETURN_1^# { return 1 }
}
// GENPROTO_1-DAG: Decl[AssociatedType]/CurrNominal:   Assoc;
// GENPROTO_1-DAG: Keyword/None:                       Type[#Self.I.Type#];
