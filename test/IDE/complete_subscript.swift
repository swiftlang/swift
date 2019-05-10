// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METATYPE_UNRESOLVED | %FileCheck %s -check-prefix=METATYPE_UNRESOLVED
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METATYPE_INT | %FileCheck %s -check-prefix=METATYPE_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_INT | %FileCheck %s -check-prefix=INSTANCE_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METATYPE_ARCHETYPE | %FileCheck %s -check-prefix=METATYPE_ARCHETYPE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_ARCHETYPE | %FileCheck %s -check-prefix=INSTANCE_ARCHETYPE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METATYPE_LABEL | %FileCheck %s -check-prefix=METATYPE_LABEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_LABEL  | %FileCheck %s -check-prefix=INSTANCE_LABEL

struct MyStruct<T> {
  static subscript(x: Int, static defValue: T) -> MyStruct<T> {
    fatalError()
  }
  subscript(x: Int, instance defValue: T) -> Int {
    fatalError()
  }
}

func test1() {
  let _ = MyStruct #^METATYPE_UNRESOLVED^#
// METATYPE_UNRESOLVED: Begin completions, 4 items
// METATYPE_UNRESOLVED-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#static: _#}][#MyStruct<_>#];
// METATYPE_UNRESOLVED-DAG: Decl[Constructor]/CurrNominal:      ()[#MyStruct<_>#];
// METATYPE_UNRESOLVED-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<_>.Type#];
// METATYPE_UNRESOLVED-DAG: Keyword/CurrNominal:                .Type[#MyStruct<_>.Type#];
// METATYPE_UNRESOLVED: End completions


  let _ = MyStruct<Int> #^METATYPE_INT^#
// METATYPE_INT: Begin completions, 4 items
// METATYPE_INT-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#static: Int#}][#MyStruct<Int>#];
// METATYPE_INT-DAG: Decl[Constructor]/CurrNominal:      ()[#MyStruct<Int>#];
// METATYPE_INT-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<Int>.Type#];
// METATYPE_INT-DAG: Keyword/CurrNominal:                .Type[#MyStruct<Int>.Type#];
// METATYPE_INT: End completions

  let _ = MyStruct<Int>()#^INSTANCE_INT^#
// INSTANCE_INT: Begin completions, 2 items
// INSTANCE_INT-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#instance: Int#}][#Int#];
// INSTANCE_INT-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<Int>#];
// INSTANCE_INT: End completions

}
func test2<U>(value: MyStruct<U>) {
  let _ = MyStruct<U>#^METATYPE_ARCHETYPE^#
// METATYPE_ARCHETYPE: Begin completions, 4 items
// METATYPE_ARCHETYPE-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#static: U#}][#MyStruct<U>#];
// METATYPE_ARCHETYPE-DAG: Decl[Constructor]/CurrNominal:      ()[#MyStruct<U>#];
// METATYPE_ARCHETYPE-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<U>.Type#];
// METATYPE_ARCHETYPE-DAG: Keyword/CurrNominal:                .Type[#MyStruct<U>.Type#];
// METATYPE_ARCHETYPE: End completions

  let _ = value #^INSTANCE_ARCHETYPE^#
// INSTANCE_ARCHETYPE: Begin completions, 2 items
// INSTANCE_ARCHETYPE-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#instance: U#}][#Int#];
// INSTANCE_ARCHETYPE-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<U>#];
// INSTANCE_ARCHETYPE: End completions

  let _ = MyStruct<U>[42, #^METATYPE_LABEL^#
// METATYPE_LABEL: Begin completions, 1 items
// METATYPE_LABEL-DAG: Keyword/ExprSpecific:               static: [#Argument name#];
// METATYPE_LABEL: End completions

  let _ = value[42, #^INSTANCE_LABEL^#
// INSTANCE_LABEL: Begin completions, 1 items
// INSTANCE_LABEL-DAG: Keyword/ExprSpecific:               instance: [#Argument name#];
// INSTANCE_LABEL: End completions
}
