// RUN: %batch-code-completion

func overloaded(content: () -> Int) {}
func overloaded(@MyResultBuilder stuff: () -> Int) {}

@resultBuilder struct MyResultBuilder {
  static func buildExpression(_ content: Int) -> Int { content }
  static func buildBlock() -> Int { 4 }
}

struct HStack {
  init(spacing: Double, @MyResultBuilder content: () -> Int) {}
  func qadding(_ length: Double) { }
}

func test() {
  overloaded {
    HStack(spacing: #^COMPLETE^#) {}
      .qadding(32)
  }
}

// COMPLETE: Literal[Integer]/None/TypeRelation[Convertible]: 0[#Double#]; name=0
