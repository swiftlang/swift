// {"kind":"typecheck","signature":"swift::constraints::getLoc(swift::ASTNode)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue : b var projectedValue init(projectedValue c) {
    func d(@a Int) d
