// {"kind":"typecheck","original":"8d81c2b7","signature":"swift::TypeBase::getExistentialLayout()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"checkDistributedTargetResultType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a<d: DistributedActorSystem> {
  typealias ActorSystem = d
  distributed func b() -> c
}
