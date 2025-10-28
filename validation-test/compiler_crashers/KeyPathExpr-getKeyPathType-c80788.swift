// {"kind":"typecheck","original":"6bb0b020","signature":"swift::KeyPathExpr::getKeyPathType() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{ let a = \ .b { c} (let c = a
