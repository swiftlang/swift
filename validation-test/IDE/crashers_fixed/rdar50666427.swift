// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

struct SD {
  var array: [Int] = []
}

func test(sd: SD) {
  _ = sd[\.array[#^COMPLETE^#]]
}
