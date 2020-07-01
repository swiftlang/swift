// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE1 | %FileCheck %s --check-prefix=CHECK
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE1 | %FileCheck %s --check-prefix=CHECK

@_functionBuilder
struct Builder {
  static func buildBlock<T1>(_ v1: T1) -> T1 { v1 }
  static func buildBlock<T1, T2>(_ v1: T1, _ v2: T2) -> (T1, T2) { v1 }
}

struct MyValue {
  var title: String
  var id: Int
  var value: Float
}

func build<T>(@Builder fn: (MyValue) -> T) {}

struct Container {
  init(x: Float) {}
  init(x: Int) {}
}

func test(foo: Foo) {
  build { val in
    Container(x: val.#^COMPLETE1^#)
  }
  build { val in
    1 + 2
    Container(x: val.#^COMPLETE2^#)
  }
}

// CHECK: Begin completions, 4 items
// CHECK: Keyword[self]/CurrNominal:          self[#MyValue#]; name=self
// CHECK: Decl[InstanceVar]/CurrNominal:      title[#String#]; name=title
// CHECK: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]: id[#Int#]; name=id
// CHECK: Decl[InstanceVar]/CurrNominal/TypeRelation[Identical]: value[#Float#]; name=value
// CHECK: End completions
