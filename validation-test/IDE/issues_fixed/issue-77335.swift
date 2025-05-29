// RUN: %swift-ide-test -code-completion -code-completion-token COMPLETE -source-filename %s

// https://github.com/swiftlang/swift/issues/77335
// Make sure we don't crash

func foo(_ x: X!) {
  x.#^COMPLETE^#
}
