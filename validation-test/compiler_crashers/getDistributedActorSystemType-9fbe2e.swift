// {"kind":"typecheck","original":"4ddade84","signature":"swift::getDistributedActorSystemType(swift::NominalTypeDecl*)","signatureAssert":"Assertion failed: (!isInvalid()), function getTypeWitnessByName","signatureNext":"TypeChecker::checkDistributedActor"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
@attached(extension conformances: DistributedActor) macro a()
@a distributed actor b {
  distributed init()
}
