// {"kind":"typecheck","original":"c1399da0","signature":"swift::FunctionType::FunctionType(llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Yield>, swift::Type, std::__1::optional<swift::ASTExtInfo>, swift::ASTContext const*, swift::RecursiveTypeProperties)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType","signatureNext":"FunctionType::get"}
// RUN: not --crash %target-swift-frontend -typecheck %s
macro a(b: isolated ())
