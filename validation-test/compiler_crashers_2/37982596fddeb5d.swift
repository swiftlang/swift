// {"signature":"swift::TypeBase::getOptionalObjectType()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b } {
let:
  String->a = a.b
