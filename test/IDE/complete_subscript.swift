// RUN: %batch-code-completion

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
// METATYPE_UNRESOLVED-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ()[#MyStruct<_>#];
// METATYPE_UNRESOLVED-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<_>.Type#];
// METATYPE_UNRESOLVED-DAG: Keyword/CurrNominal:                .Type[#MyStruct<_>.Type#];

  let _ = MyStruct[#^METATYPE_UNRESOLVED_BRACKET^#
// METATYPE_UNRESOLVED_BRACKET-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]:        ['[']{#(x): Int#}, {#static: T#}[']'][#MyStruct<T>#];

  let _ = MyStruct<Int> #^METATYPE_INT^#
// METATYPE_INT: Begin completions, 4 items
// METATYPE_INT-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#static: Int#}][#MyStruct<Int>#];
// METATYPE_INT-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ()[#MyStruct<Int>#];
// METATYPE_INT-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<Int>.Type#];
// METATYPE_INT-DAG: Keyword/CurrNominal:                .Type[#MyStruct<Int>.Type#];

  let _ = MyStruct<Int>[#^METATYPE_INT_BRACKET^#
// METATYPE_INT_BRACKET-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]:        ['[']{#(x): Int#}, {#static: Int#}[']'][#MyStruct<Int>#];

  let _ = MyStruct<Int>()#^INSTANCE_INT^#
// INSTANCE_INT: Begin completions, 2 items
// INSTANCE_INT-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#instance: Int#}][#Int#];
// INSTANCE_INT-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<Int>#];

  let _ = MyStruct<Int>()[#^INSTANCE_INT_BRACKET^#
// INSTANCE_INT_BRACKET-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]:        ['[']{#(x): Int#}, {#instance: Int#}[']'][#Int#];
// INSTANCE_INT_BRACKET-DAG: Pattern/CurrNominal/Flair[ArgLabels]:                ['[']{#keyPath: KeyPath<MyStruct<Int>, Value>#}[']'][#Value#];
}
func test2<U>(value: MyStruct<U>) {
  let _ = MyStruct<U>#^METATYPE_ARCHETYPE^#
// METATYPE_ARCHETYPE: Begin completions, 4 items
// METATYPE_ARCHETYPE-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#static: U#}][#MyStruct<U>#];
// METATYPE_ARCHETYPE-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ()[#MyStruct<U>#];
// METATYPE_ARCHETYPE-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<U>.Type#];
// METATYPE_ARCHETYPE-DAG: Keyword/CurrNominal:                .Type[#MyStruct<U>.Type#];

  let _ = MyStruct<U>[#^METATYPE_ARCHETYPE_BRACKET^#
// METATYPE_ARCHETYPE_BRACKET-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]:        ['[']{#(x): Int#}, {#static: U#}[']'][#MyStruct<U>#];

  let _ = value #^INSTANCE_ARCHETYPE^#
// INSTANCE_ARCHETYPE: Begin completions, 2 items
// INSTANCE_ARCHETYPE-DAG: Decl[Subscript]/CurrNominal:        [{#(x): Int#}, {#instance: U#}][#Int#];
// INSTANCE_ARCHETYPE-DAG: Keyword[self]/CurrNominal:          .self[#MyStruct<U>#];

  let _ = value[#^INSTANCE_ARCHETYPE_BRACKET^#
// INSTANCE_ARCHETYPE_BRACKET-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]:        ['[']{#(x): Int#}, {#instance: U#}[']'][#Int#];
// INSTANCE_ARCHETYPE_BRACKET-DAG: Pattern/CurrNominal/Flair[ArgLabels]:                ['[']{#keyPath: KeyPath<MyStruct<U>, Value>#}[']'][#Value#];

  let _ = MyStruct<U>[42, #^METATYPE_LABEL^#
// METATYPE_LABEL: Begin completions, 1 items
// METATYPE_LABEL-DAG: Pattern/Local/Flair[ArgLabels]: {#static: U#}[#U#];

  let _ = value[42, #^INSTANCE_LABEL^#
// INSTANCE_LABEL: Begin completions, 1 items
// INSTANCE_LABEL-DAG: Pattern/Local/Flair[ArgLabels]: {#instance: U#}[#U#];
}

class Base {
  static subscript(static x: Int) -> Int { return 1 }
  subscript(instance x: Int) -> Int { return 1 }
}
class Derived: Base {
  static subscript(derivedStatic x: Int) -> Int { return 1 }
  subscript(derivedInstance x: Int) -> Int { return 1 }

  func testInstance() {
    let _ = self[#^SELF_IN_INSTANCEMETHOD^#]
// SELF_IN_INSTANCEMETHOD: Begin completions, 3 items
// SELF_IN_INSTANCEMETHOD-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]: ['[']{#derivedInstance: Int#}[']'][#Int#];
// SELF_IN_INSTANCEMETHOD-DAG: Decl[Subscript]/Super/Flair[ArgLabels]:       ['[']{#instance: Int#}[']'][#Int#];
// SELF_IN_INSTANCEMETHOD-DAG: Pattern/CurrNominal/Flair[ArgLabels]:         ['[']{#keyPath: KeyPath<Derived, Value>#}[']'][#Value#];

    let _ = super[#^SUPER_IN_INSTANCEMETHOD^#]
// SUPER_IN_INSTANCEMETHOD: Begin completions, 2 items
// SUPER_IN_INSTANCEMETHOD-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]: ['[']{#instance: Int#}[']'][#Int#];
// SUPER_IN_INSTANCEMETHOD-DAG: Pattern/CurrNominal/Flair[ArgLabels]:         ['[']{#keyPath: KeyPath<Base, Value>#}[']'][#Value#];
  }

  static func testStatic() {
    let _ = self[#^SELF_IN_STATICMETHOD^#]
// SELF_IN_STATICMETHOD: Begin completions, 3 items
// SELF_IN_STATICMETHOD-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]: ['[']{#derivedStatic: Int#}[']'][#Int#];
// SELF_IN_STATICMETHOD-DAG: Decl[Subscript]/Super/Flair[ArgLabels]:       ['[']{#static: Int#}[']'][#Int#];
// SELF_IN_STATICMETHOD-DAG: Pattern/CurrNominal/Flair[ArgLabels]:         ['[']{#keyPath: KeyPath<Derived.Type, Value>#}[']'][#Value#];

    let _ = super[#^SUPER_IN_STATICMETHOD^#]
// SUPER_IN_STATICMETHOD: Begin completions, 2 items
// SUPER_IN_STATICMETHOD-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]: ['[']{#static: Int#}[']'][#Int#];
// SUPER_IN_STATICMETHOD-DAG: Pattern/CurrNominal/Flair[ArgLabels]:         ['[']{#keyPath: KeyPath<Base.Type, Value>#}[']'][#Value#];
  }
}

struct MyStruct1<X: Comparable> {
  subscript(idx1 _: Int, idx2 _: X) -> Int! { return 1 }
}
func testSubscriptCallSig<T>(val: MyStruct1<T>) {
  val[#^LABELED_SUBSCRIPT^#
// LABELED_SUBSCRIPT: Begin completions, 2 items
// LABELED_SUBSCRIPT-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]:        ['[']{#idx1: Int#}, {#idx2: Comparable#}[']'][#Int!#];
// LABELED_SUBSCRIPT-DAG: Pattern/CurrNominal/Flair[ArgLabels]:                ['[']{#keyPath: KeyPath<MyStruct1<T>, Value>#}[']'][#Value#];
}

func testSubcscriptTuple(val: (x: Int, String)) {
  val[#^TUPLE^#]
// TUPLE: Begin completions, 1 items
// TUPLE-DAG: Pattern/CurrNominal/Flair[ArgLabels]:                 ['[']{#keyPath: KeyPath<(x: Int, String), Value>#}[']'][#Value#];
}

struct HasSettableSub {
    subscript(a: String) -> Int {
        get { return 1 }
        set { }
    }
}

func testSettableSub(x: inout HasSettableSub) {
    let local = "some string"
    x[#^SETTABLE_SUBSCRIPT^#] = 32
}
// SETTABLE_SUBSCRIPT-DAG: Pattern/CurrNominal/Flair[ArgLabels]: ['[']{#keyPath: KeyPath<HasSettableSub, Value>#}[']'][#Value#];
// SETTABLE_SUBSCRIPT-DAG: Decl[Subscript]/CurrNominal/Flair[ArgLabels]: ['[']{#(a): String#}[']'][#@lvalue Int#];
// SETTABLE_SUBSCRIPT-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: local[#String#]; name=local

// rdar://139333904 - Make sure we don't hit an assertion.
func testFnKeyPathSubscript() {
  // The keyPath: subscript is supported on all non-nominal types, including functions.
  test1[#^FN_KEYPATH_SUBSCRIPT^#]
  // FN_KEYPATH_SUBSCRIPT: Pattern/None/Flair[ArgLabels]: ['[']{#keyPath: KeyPath<() -> (), Value>#}[']'][#Value#]; name=keyPath:
}
