// {"kind":"typecheck","original":"77effdcf","signature":"swift::TypeBase::getSuperclassForDecl(swift::ClassDecl const*, bool)","signatureAssert":"Assertion failed: (isa<ClassDecl>(nominalDecl) && \"expected a class here\"), function getSuperclassForDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: b
  protocol c {
    associatedtype d
  }
  class b: c
    struct e: a {  f: d
