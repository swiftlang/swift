// {"kind":"complete","signature":"swift::MacroDefinitionRequest::evaluate(swift::Evaluator&, swift::MacroDecl*) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a = ( @freestanding macro b = a#^COMPLETE^#
