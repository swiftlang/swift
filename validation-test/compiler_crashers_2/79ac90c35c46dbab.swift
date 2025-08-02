// {"kind":"typecheck","signature":"(anonymous namespace)::TypeJoin::join(swift::CanType, swift::CanType)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a protocol b < c { associatedtype c }
                                 [ b<a>, a & b
