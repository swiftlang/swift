// {"kind":"typecheck","original":"08496a0b","signature":"swift::TypeBase::getSuperclassForDecl(swift::ClassDecl const*, bool)","signatureAssert":"Assertion failed: (baseClass->isSuperclassOf(currentClass) && \"no inheritance relationship between given classes\"), function getSuperclassForDecl","signatureNext":"TypeBase::getContextSubstitutions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b {
  @globalActor actor d {
    static shared = @d
    class c
      class e : c
