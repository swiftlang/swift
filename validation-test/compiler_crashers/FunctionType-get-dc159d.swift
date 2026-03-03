// {"kind":"typecheck","signature":"swift::FunctionType::get(llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a {
  case (b: isolated c)
}
