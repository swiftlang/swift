// {"kind":"typecheck","signature":"(anonymous namespace)::TypeJoin::visitProtocolCompositionType(swift::CanType)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a protocol b < c { associatedtype c }
                                 [ b<a>, a & b
