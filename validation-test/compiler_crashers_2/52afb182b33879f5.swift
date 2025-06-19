// {"signature":"swift::TypeBase::getSuperclassForDecl(swift::ClassDecl const*, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  typealias b = c class e : a protocol d : e, d.b
