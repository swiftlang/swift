// RUN: %batch-code-completion

@resultBuilder struct MyBuilder {
  static func buildBlock(_ components: [Int]...) -> [Int]
  static func buildFinalResult(_ component: [Int]) -> Int
}

func build(@MyBuilder itemsBuilder: () -> Int) {}

func test() {
  let modifiers = build {
    for modifier in invalid {
    }
  }

  modifiers.#^COMPLETE^#
}

// COMPLETE: Keyword[self]/CurrNominal:          self[#Void#]; name=self
