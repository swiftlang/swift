// RUN: %target-swift-ide-test --conforming-methods -code-completion-token=COMPLETE --conforming-methods-expected-types=s:14swift_ide_test10MySequenceP -source-filename %s

protocol MySequence {
  associatedtype Element
}

struct Foo<X>: MySequence {
	typealias Element = X
}

struct ArgumentDefinition {
  fileprivate func bashCompletionWords() -> Foo<String> { fatalError() }
}

func myFlatMap<SegmentOfResult: MySequence>(_ transform: (ArgumentDefinition) -> SegmentOfResult) -> Foo<SegmentOfResult.Element> {
  fatalError()
}

func generateArgumentWords() {
  _ = myFlatMap { $0.#^COMPLETE^# } as Foo<String>
}
