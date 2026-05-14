// {"kind":"typecheck","original":"860f33ae","signature":"swift::TypeChecker::lookupUnqualified(swift::DeclContext*, swift::DeclNameRef, swift::SourceLoc, swift::optionset::OptionSet<swift::NameLookupFlags, unsigned int>)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"DerivedConformance::deriveDistributedActor"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
class a<b> {
  distributed actor DefaultDistributedActorSystem {
  }
}
