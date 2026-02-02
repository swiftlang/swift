// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// https://github.com/swiftlang/swift/issues/65817
protocol Publisher<Output, Failure> {
  associatedtype Output
  associatedtype Failure : Error
}
func pnReceive<P: Publisher>(_ publisher: P) where P.Failure == Never
func test() {
  pnReceive(#^COMPLETE^#) { }
}
