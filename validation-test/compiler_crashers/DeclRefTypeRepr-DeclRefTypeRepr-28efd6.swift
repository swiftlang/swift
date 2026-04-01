// {"kind":"typecheck","original":"a8b51f73","signature":"swift::DeclRefTypeRepr::DeclRefTypeRepr(swift::TypeReprKind, swift::DeclNameRef, swift::DeclNameLoc, unsigned int, bool)","signatureAssert":"Assertion failed: (Name.isSimpleName() && !Name.isSpecial() && !Name.isOperator()), function DeclRefTypeRepr","signatureNext":"DeclRefTypeRepr::create"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a() -> ::init
