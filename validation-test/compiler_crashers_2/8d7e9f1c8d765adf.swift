// {"kind":"typecheck","original":"3ea3fc6a","signature":"swift::DeclRefTypeRepr::overwriteNameRef(swift::DeclNameRef)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch {
case a. init(
