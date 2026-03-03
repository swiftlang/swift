// {"kind":"complete","signature":"swift::ide::CodeCompletionResultType::calculateTypeRelation(swift::ide::ExpectedTypeContext const*, swift::DeclContext const*, swift::ide::USRBasedTypeContext const*) const"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func b(a : Array<Int>) {
let: Array = a.prefix( #^^#
