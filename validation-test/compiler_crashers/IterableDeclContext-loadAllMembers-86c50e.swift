// {"kind":"typecheck","original":"c6a502fd","signature":"swift::IterableDeclContext::loadAllMembers() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"IterableDeclContext::getMembers"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@freestanding(declaration) macro a()
@attached(extension) macro b() =
  #a
@b struct c {
}
