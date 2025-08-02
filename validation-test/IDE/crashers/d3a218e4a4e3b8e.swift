// {"kind":"complete","signature":"swift::Parser::performIDEInspectionSecondPassImpl(swift::IDEInspectionDelayedDeclState&)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
defer{ var #^COMPLETE^#
