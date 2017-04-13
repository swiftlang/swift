// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

func matched<C: Collection>(atStartOf c: C)
  where C.Index#^A^# == Index {
}
