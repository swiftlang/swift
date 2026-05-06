// {"kind":"typecheck","original":"66895c35","signature":"swift::TypeBase::getExistentialLayout()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"checkDistributedSerializationRequirementIsExactlyCodable"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a<ActorSystem: DistributedActorSystem> {
  distributed func b(c: <#type#>)
}
