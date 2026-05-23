// {"kind":"complete","original":"26aa1d68","signature":"swift::ide::CodeCompletionResultType::calculateTypeRelation(swift::ide::ExpectedTypeContext const*, swift::DeclContext const*, swift::ide::USRBasedTypeContext const*) const","signatureAssert":"Assertion failed: (!Ty->hasUnboundGenericType() && !ExpectedTy->hasUnboundGenericType()), function calculateTypeRelation","signatureNext":"ContextFreeCodeCompletionResult::calculateContextualTypeRelation"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b{
struct c<d {
typealias c = a<d>.c
e<f> -> f {
c#^^#
