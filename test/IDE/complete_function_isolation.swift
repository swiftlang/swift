// RUN: %batch-code-completion

// REQUIRES: concurrency

func test1(_ x: () -> Void) {
  x.#^NORMAL_FN^#
  // NORMAL_FN:     Begin completions, 2 items
  // NORMAL_FN-DAG: Keyword[self]/CurrNominal: self[#() -> Void#]; name=self
  // NORMAL_FN-DAG: Pattern/CurrModule/Flair[ArgLabels]/Erase[1]: ()[#Void#]; name=()
}

func test2(_ x: @isolated(any) () -> Void) {
  x.#^ISOLATED_ANY_FN^#
  // ISOLATED_ANY_FN:     Begin completions, 3 items
  // ISOLATED_ANY_FN-DAG: Pattern/CurrNominal: isolation[#(any Actor)?#]; name=isolation
  // ISOLATED_ANY_FN-DAG: Keyword[self]/CurrNominal: self[#@isolated(any) () -> Void#]; name=self
  // ISOLATED_ANY_FN-DAG: Pattern/CurrModule/Flair[ArgLabels]/Erase[1]: ()[#Void#]; name=()
}

func test3(_ x: (@isolated(any) () -> Void)?) {
  x.#^ISOLATED_ANY_OPTIONAL_FN^#
  // ISOLATED_ANY_OPTIONAL_FN-DAG: Pattern/CurrNominal/Erase[1]: ?.isolation[#(any Actor)?#]; name=isolation
  // ISOLATED_ANY_OPTIONAL_FN-DAG: Keyword[self]/CurrNominal: self[#(@isolated(any) () -> Void)?#]; name=self
}
