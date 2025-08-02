// {"kind":"typecheck","original":"3ea3fc6a","signature":"swift::DeclRefTypeRepr::DeclRefTypeRepr(swift::TypeReprKind, swift::DeclNameRef, swift::DeclNameLoc, unsigned int, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch {
case a. init(
