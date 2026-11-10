// {"kind":"typecheck","original":"5b390637","signature":"(anonymous namespace)::ExprRewriter::openExistentialReference(swift::Expr*, swift::ExistentialArchetypeType*, swift::ValueDecl*, swift::SourceLoc, bool)","signatureAssert":"Assertion failed: (baseTy->isAnyExistentialType() && \"Type must be existential\"), function openExistentialReference","signatureNext":"ExprRewriter::coerceCallArguments"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b(inout some a)
  func c(d : inout a) {
    b( ( &d
