// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t -code-complete-call-pattern-heuristics -disable-objc-attr-requires-foundation-module

// SE-0353
struct ConstraintedExistentialTest {
  protocol Producer<Event> {
    associatedtype Event
  }

  struct StringProducer: Producer {
    typealias Event = String
  }

  struct IntProducer: Producer {
    typealias Event = Int
  }

  func takeStringProducer(producer: any Producer<String>) {}

  let intProducer: IntProducer
  let stringProducer: StringProducer

  func test() {
    takeStringProducer(self.#^IN_CONSTRAINTED_EXISTENTIAL_CONTEXT^#)
  }
// IN_CONSTRAINTED_EXISTENTIAL_CONTEXT: Begin completions
// IN_CONSTRAINTED_EXISTENTIAL_CONTEXT-DAG: Decl[InstanceVar]/CurrNominal:      intProducer[#IntProducer#];
// IN_CONSTRAINTED_EXISTENTIAL_CONTEXT-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: stringProducer[#StringProducer#];
// IN_CONSTRAINTED_EXISTENTIAL_CONTEXT: End completions
}
