// {"kind":"typecheck","original":"369523c9","signature":"swift::TypeChecker::checkContextualRequirements(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, swift::GenericSignature)","signatureAssert":"Assertion failed: (nominalParent->getGenericSignature().getGenericParams().size() == genericSig.getGenericParams().size() && \"expected nested type to be non-generic\"), function checkContextualRequirements","signatureNext":"TypeResolver::applyGenericArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  typealias b<c, d> = a
  extension b<Float, Int> {
    extension b {
    }
  }
}
