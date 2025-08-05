// {"kind":"complete","signature":"swift::constraints::TypeVarRefCollector::walkToStmtPre(swift::Stmt*)","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
a -> { let b = switch { case return } func c -> #^COMPLETE^#
