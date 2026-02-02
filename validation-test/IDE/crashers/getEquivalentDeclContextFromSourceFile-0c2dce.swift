// {"issueID":84785,"kind":"complete","signature":"(anonymous namespace)::getEquivalentDeclContextFromSourceFile(swift::DeclContext*, swift::SourceFile*)","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
// https://github.com/swiftlang/swift/issues/84785
func 0 {
#^^#
}
do { func a {
#^b^#
