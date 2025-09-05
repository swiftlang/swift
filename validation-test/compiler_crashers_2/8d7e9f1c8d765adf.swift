// {"kind":"typecheck","original":"3ea3fc6a","signature":"swift::DeclRefTypeRepr::DeclRefTypeRepr(swift::TypeReprKind, swift::DeclNameRef, swift::DeclNameLoc, unsigned int, bool)","signatureAssert":"Assertion failed: (Name.isSimpleName() && !Name.isSpecial() && !Name.isOperator()), function DeclRefTypeRepr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch {
case a. init(
