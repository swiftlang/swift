// {"kind":"typecheck","original":"bc1e9cdf","signature":"(anonymous namespace)::AssociatedTypeInference::solve()","signatureAssert":"Assertion failed: (!hasTypeParameter() && \"already have an interface type\"), function mapTypeOutOfEnvironment"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed actor Distributed<DefaultDistributedActorSystem> {
}
