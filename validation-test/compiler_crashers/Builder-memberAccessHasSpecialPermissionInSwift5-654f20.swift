// {"kind":"typecheck","original":"9d84356d","signature":"swift::ActorReferenceResult::Builder::memberAccessHasSpecialPermissionInSwift5(swift::DeclContext const*, swift::ReferencedActor&, swift::ValueDecl const*, swift::SourceLoc, std::__1::optional<swift::VarRefUseEnv>)","signatureAssert":"Assertion failed: (fn->hasAsync()), function wasLegacyEscapingUseRestriction","signatureNext":"ActorReferenceResult::Builder::checkedByFlowIsolation"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a {
  b
  init(isolated a ) {
    b
