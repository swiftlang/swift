// {"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, swift::SILArgument const*, llvm::function_ref<void (swift::SILPrintContext&)>)"}
// RUN: not %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a<ActorSystem: DistributedActorSystem> {
}
extension a {
  nonisolated var id: ActorSystem.ActorID {
  }
}
