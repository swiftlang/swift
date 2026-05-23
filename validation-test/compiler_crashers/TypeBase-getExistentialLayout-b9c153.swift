// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"f59eddf5","signature":"swift::TypeBase::getExistentialLayout()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"deriveBodyDistributed_invokeHandlerOnReturn"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
// REQUIRES: OS=macosx
import Distributed
struct a: DistributedActorSystem {
  typealias ResultHandler = b
  class b: DistributedTargetInvocationResultHandler {
    typealias SerializationRequirement = Codable
    func onReturn<c: Codable>(value: c) async throws
  }
}
