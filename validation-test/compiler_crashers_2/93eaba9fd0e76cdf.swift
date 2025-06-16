// {"signature":"diagnoseDictionaryLiteralDuplicateKeyEntries(swift::Expr const*, swift::DeclContext const*)::DiagnoseWalker::walkToExprPre(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[ 1.01: "" 1.01: ""
