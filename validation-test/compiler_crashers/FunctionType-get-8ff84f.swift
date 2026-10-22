// {"kind":"typecheck","original":"9b9d5092","signature":"swift::FunctionType::get(llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Yield>, swift::Type, std::__1::optional<swift::ASTExtInfo>)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType","signatureNext":"InterfaceTypeRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
macro a(actor: isolated b)
