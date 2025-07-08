// {"kind":"complete","signature":"checkLabeledStmtShadowing(swift::ASTContext&, swift::SourceFile*, swift::LabeledStmt*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
let:{ let a=
#^COMPLETE^#{
b:{
