// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

func _thread() {
  _ = #^A^#
}

func run<InputCollection : Collection, Result>(_: InputCollection) -> Result {}
