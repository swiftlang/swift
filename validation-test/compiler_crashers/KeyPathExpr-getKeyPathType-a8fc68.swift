// {"kind":"typecheck","original":"7048f9d7","signature":"swift::KeyPathExpr::getKeyPathType() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  convenience #a(\array)
}
