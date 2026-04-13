// {"kind":"typecheck","original":"e472bcf2","signature":"swift::GenericFunctionType::get(swift::GenericSignature, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function GenericFunctionType","signatureNext":"InterfaceTypeRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
macro a<b>(actor: isolated c)
