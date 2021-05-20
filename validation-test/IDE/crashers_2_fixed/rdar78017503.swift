// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

@resultBuilder
struct TupleBuilder<T> {
  static func buildBlock() -> () { }
}

func testPatternMatching() {
  @TupleBuilder<String> var x3 {
    let x: Int? = nil
    #^COMPLETE^#
  }
}
