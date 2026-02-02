// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename %s

func test() {
  let cl = { arg in
    let name = arg as String
    #^A^#
  }
}
