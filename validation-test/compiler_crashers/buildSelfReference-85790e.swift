// {"kind":"typecheck","original":"6fc8e959","signature":"swift::buildSelfReference(swift::VarDecl*, swift::SelfAccessorKind, bool, swift::Type)","signatureAssert":"Assertion failed: (convertTy->castTo<MetatypeType>() ->getInstanceType() ->isExactSuperclassOf(selfTy->getMetatypeInstanceType())), function buildSelfReference","signatureNext":"buildStorageReference"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  class var b: Int {
    class c: a<> {
      class var b: Int {
        didSet
      }
    }
  }
}
