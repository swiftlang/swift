// {"kind":"typecheck","original":"0736c17c","signature":"swift::ActorReferenceResult::Builder::checkedByFlowIsolation(swift::DeclContext const*, swift::ReferencedActor&, swift::ValueDecl*, swift::SourceLoc, std::__1::optional<swift::VarRefUseEnv>)","signatureNext":"ActorReferenceResult::Builder::build"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a {
b
  nonisolated(nonsending) init async {
    b
