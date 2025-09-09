// {"signature":"void (anonymous namespace)::StmtChecker::checkSiblingCaseStmts<swift::CaseStmt* const*>(swift::CaseStmt* const*, swift::CaseStmt* const*, swift::CaseParentKind, bool&, swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a func b(c : a) -> Int {
  switch
    c {
    case (let \d)d
