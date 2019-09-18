// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_1 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BAR_OBJECT_DOT_1 | %FileCheck %s -check-prefix=BAR_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CATCHSEQUENCE_DOT | %FileCheck %s -check-prefix=CATCHSEQUENCE_DOT

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
// FOO_OBJECT_DOT-NEXT: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// FOO_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:      instanceProperty[#Int#]
// FOO_OBJECT_DOT-NEXT: End completions

struct BarStruct : FooEvenMoreRefinedProtocol {
  var instanceProperty: Int { return 0 }
}
// BAR_OBJECT_DOT: Begin completions
// BAR_OBJECT_DOT-NEXT: Keyword[self]/CurrNominal: self[#BarStruct#]; name=self
// BAR_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:      instanceProperty[#Int#]
// BAR_OBJECT_DOT-NEXT: End completions

func test(a: FooStruct) {
  a.#^FOO_OBJECT_DOT_1^#
}

func test(a: BarStruct) {
  a.#^BAR_OBJECT_DOT_1^#
}

protocol ObservableConvertibleType {
    associatedtype T
}
class Observable<T> : ObservableConvertibleType {}
class CatchSequence<S: Sequence>: Observable<S.Element.T> where S.Element: ObservableConvertibleType {}

extension ObservableConvertibleType {
    static func catchError() -> Observable<T> {
        return CatchSequence.#^CATCHSEQUENCE_DOT^#
    }
}
// CATCHSEQUENCE_DOT: Begin completions
// CATCHSEQUENCE_DOT-DAG: Keyword[self]/CurrNominal:          self[#CatchSequence<_>.Type#]; name=self
// CATCHSEQUENCE_DOT-DAG: Keyword/CurrNominal:                Type[#CatchSequence<_>.Type#]; name=Type
// CATCHSEQUENCE_DOT-DAG: Decl[Constructor]/CurrNominal:      init()[#CatchSequence<_>#]; name=init()
// CATCHSEQUENCE_DOT-DAG: Decl[StaticMethod]/Super:           catchError()[#Observable<CatchSequence<_>.T>#]; name=catchError()
// CATCHSEQUENCE_DOT-DAG: Decl[TypeAlias]/Super:              T[#T#]; name=T
// CATCHSEQUENCE_DOT: End completions
