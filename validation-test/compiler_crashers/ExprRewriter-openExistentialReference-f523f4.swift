// {"kind":"typecheck","original":"5b390637","signature":"(anonymous namespace)::ExprRewriter::openExistentialReference(swift::Expr*, swift::ExistentialArchetypeType*, swift::ValueDecl*, swift::SourceLoc)","signatureAssert":"Assertion failed: (baseTy->isAnyExistentialType() && \"Type must be existential\"), function openExistentialReference","signatureNext":"ExprRewriter::coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b(inout some a)
  func c(d : inout a) {
    b( ( &d
