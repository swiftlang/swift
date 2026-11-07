// {"kind":"typecheck","original":"a8b51f73","signature":"swift::DeclRefTypeRepr::DeclRefTypeRepr(swift::TypeReprKind, swift::DeclNameRef, swift::DeclNameLoc, unsigned int, bool)","signatureAssert":"Assertion failed: (Name.isSimpleName() && !Name.isSpecial() && !Name.isOperator()), function DeclRefTypeRepr","signatureNext":"DeclRefTypeRepr::create"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a() -> ::init
