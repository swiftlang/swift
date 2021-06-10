// RUN: %target-swift-ide-test --conforming-methods -code-completion-token=COMPLETE_EXPR --conforming-methods-expected-types=s:14swift_ide_test10MySequenceP -source-filename %s
// RUN: %target-swift-ide-test --conforming-methods -code-completion-token=COMPLETE_STMT --conforming-methods-expected-types=s:14swift_ide_test10MySequenceP -source-filename %s

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
  // Explicitly coerce the type using 'as'. This is type checked as an expression.
  _ = myFlatMap { $0.#^COMPLETE_EXPR^# } as Foo<String>
}

func generateArgumentWords() -> Foo<String> {
  // Implicitly coerce the type from the return type. This is type checked as a stmt.
  return myFlatMap { $0.#^COMPLETE_STMT^# }
}
