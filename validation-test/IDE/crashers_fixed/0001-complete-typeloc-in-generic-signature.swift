// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts

func matched<C: Collection>(atStartOf c: C)
  where C.Index#^A^# == Index {
}
