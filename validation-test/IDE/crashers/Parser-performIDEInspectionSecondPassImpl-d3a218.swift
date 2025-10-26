// {"kind":"complete","signature":"swift::Parser::performIDEInspectionSecondPassImpl(swift::IDEInspectionDelayedDeclState&)","signatureAssert":"Assertion failed: ((DC->isTypeContext() || DC->isModuleScopeContext()) && \"Delayed decl must be a type member or a top-level decl\"), function performIDEInspectionSecondPassImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
defer{ var #^COMPLETE^#
