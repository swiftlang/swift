// {"kind":"typecheck","original":"5315751e","signature":"swift::ActorReferenceResult::Builder::memberAccessHasSpecialPermissionInSwift5(swift::DeclContext const*, swift::ReferencedActor&, swift::ValueDecl const*, swift::SourceLoc, std::__1::optional<swift::VarRefUseEnv>)","signatureNext":"ActorReferenceResult::Builder::checkedByFlowIsolation"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: Actor {
  b
  deinit {
    b
  }
}
