// {"kind":"typecheck","signature":"swift::adjustFunctionTypeForConcurrency(swift::AnyFunctionType*, swift::ValueDecl*, swift::DeclContext*, unsigned int, bool, llvm::function_ref<swift::Type (swift::AbstractClosureExpr const*)>, llvm::function_ref<bool (swift::ClosureExpr const*)>, llvm::function_ref<swift::Type (swift::Type)>)","signatureAssert":"Assertion failed: (fnType->getIsolation().isNonIsolated()), function adjustFunctionTypeForConcurrency"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a func b(isolated a...) b
