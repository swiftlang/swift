// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// https://github.com/swiftlang/swift/issues/85434
protocol MyError: Error {}
func oops() {
  do {
  } catch let error as any MyError {
    switch error.httpStatus {
    case .#^COMPLETE^#
    default: throw error
    }
  }
}
