// {"kind":"typecheck","signature":"swift::TypeChecker::diagnoseRequirementFailure(swift::CheckGenericArgumentsResult::RequirementFailureInfo const&, swift::SourceLoc, swift::SourceLoc, swift::Type, llvm::ArrayRef<swift::GenericTypeParamType*>, llvm::function_ref<swift::Type (swift::SubstitutableType*)>)","signatureAssert":"Assertion failed: (errorLoc.isValid() && noteLoc.isValid()), function diagnoseRequirementFailure","signatureNext":"TypeChecker::checkIsolatedConformancesInType::IsolatedConformanceInTypeWalker::check"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct S: @MainActor Error {
}
func f(_: Result<Int, S>) {
}
