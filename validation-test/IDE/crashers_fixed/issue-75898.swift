// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// https://github.com/swiftlang/swift/issues/75898
extension Result {
  typealias Value = Success

  init(value: Value) {}

  func tryMap() {
    return flatMap { value in }#^COMPLETE^#
  }
}
