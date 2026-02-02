// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

class Foo<T> {
}

extension Foo where T: Comparable {
  func foo() {}
}

protocol P {
  typealias alias = Foo
}
protocol P {}

func Test() {
  P.alias.#^COMPLETE^#
}
