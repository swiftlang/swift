// {"kind":"complete","signature":"swift::ParseAbstractFunctionBodyRequest::evaluate(swift::Evaluator&, swift::AbstractFunctionDecl*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
defer{ var #^COMPLETE^#
