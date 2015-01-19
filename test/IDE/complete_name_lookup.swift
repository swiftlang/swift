// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_1 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BAR_OBJECT_DOT_1 | FileCheck %s -check-prefix=BAR_OBJECT_DOT

protocol FooBaseProtocol {
  var instanceProperty: Int { get }
}

protocol FooRefinedProtocol : FooBaseProtocol {}

protocol FooMoreRefinedProtocol : FooRefinedProtocol {}

protocol FooEvenMoreRefinedProtocol : FooRefinedProtocol {}

struct FooStruct : FooMoreRefinedProtocol {
  var instanceProperty: Int { return 0 }
}
// FOO_OBJECT_DOT: Begin completions
// FOO_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:      instanceProperty[#Int#]
// FOO_OBJECT_DOT-NEXT: End completions

struct BarStruct : FooEvenMoreRefinedProtocol {
  var instanceProperty: Int { return 0 }
}
// BAR_OBJECT_DOT: Begin completions
// BAR_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:      instanceProperty[#Int#]
// BAR_OBJECT_DOT-NEXT: End completions

func test(a: FooStruct) {
  a.#^FOO_OBJECT_DOT_1^#
}

func test(a: BarStruct) {
  a.#^BAR_OBJECT_DOT_1^#
}

