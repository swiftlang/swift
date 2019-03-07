// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

extension UnknownTy {
  var #^COMPLETE^#
}
