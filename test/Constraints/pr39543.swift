// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=CC

// PR #39543: Make sure we can complete in this position without crashing.
extension String {
  init(format: String, _: Any) { fatalError() }
}
extension RandomAccessCollection {
  func foo() {
    print(String(format: "", Int(distance(from:#^CC^# startIndex, to: startIndex))))
  }
}
