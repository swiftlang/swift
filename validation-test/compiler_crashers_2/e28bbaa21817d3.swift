// {"kind":"typecheck","signature":"swift::TypeBase::removeArgumentLabels(unsigned int)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a dynamic subscript(b c: Int) a subscript(b c: Int) a @_dynamicReplacement(for: subscript) subscript(Int) a { }
