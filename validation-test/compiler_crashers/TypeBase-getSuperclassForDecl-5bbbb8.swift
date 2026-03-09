// {"kind":"typecheck","original":"991c9b5d","signature":"swift::TypeBase::getSuperclassForDecl(swift::ClassDecl const*, bool)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a.b.c
class a<d {
  struct c
    protocol b: a
