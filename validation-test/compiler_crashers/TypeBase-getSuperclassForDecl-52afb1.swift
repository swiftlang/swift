// {"kind":"typecheck","signature":"swift::TypeBase::getSuperclassForDecl(swift::ClassDecl const*, bool)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  typealias b = c class e : a protocol d : e, d.b
