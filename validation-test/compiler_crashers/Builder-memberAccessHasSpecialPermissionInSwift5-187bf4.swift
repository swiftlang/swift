// {"kind":"typecheck","original":"5315751e","signature":"swift::ActorReferenceResult::Builder::memberAccessHasSpecialPermissionInSwift5(swift::DeclContext const*, swift::ReferencedActor&, swift::ValueDecl const*, swift::SourceLoc, std::__1::optional<swift::VarRefUseEnv>)","signatureNext":"ActorReferenceResult::Builder::checkedByFlowIsolation"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: Actor {
  b
  deinit {
    b
  }
}
