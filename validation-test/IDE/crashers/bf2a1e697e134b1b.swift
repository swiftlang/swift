// {"kind":"complete","signature":"swift::MacroDefinitionRequest::evaluate(swift::Evaluator&, swift::MacroDecl*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
macro a = ( @freestanding macro b = a#^COMPLETE^#
