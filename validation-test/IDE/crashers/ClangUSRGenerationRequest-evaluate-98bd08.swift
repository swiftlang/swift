// {"kind":"complete","original":"54d9bed5","signature":"swift::ClangUSRGenerationRequest::evaluate(swift::Evaluator&, swift::ValueDecl const*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@ c struct a >
#^^#
