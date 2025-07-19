// {"signature":"(anonymous namespace)::TypeJoin::visitExistentialType(swift::CanType)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a protocol b < c { associatedtype c }
                                 [ b<a>, a & b
