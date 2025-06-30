// {"kind":"complete","signature":"swift::constraints::TypeVarRefCollector::walkToStmtPre(swift::Stmt*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
a -> { let b = switch { case return } func c -> #^COMPLETE^#
