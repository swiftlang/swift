// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

// https://github.com/apple/swift/issues/51072

extension UnknownTy {
  var #^COMPLETE^#
}
