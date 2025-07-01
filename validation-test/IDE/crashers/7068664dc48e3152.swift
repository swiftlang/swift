// {"kind":"complete","signature":"swift::ide::CodeCompletionResultType::calculateTypeRelation(swift::ide::ExpectedTypeContext const*, swift::DeclContext const*, swift::ide::USRBasedTypeContext const*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
class a<b { init ? { #^COMPLETE^#
