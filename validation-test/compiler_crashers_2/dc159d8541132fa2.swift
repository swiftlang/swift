// {"kind":"typecheck","signature":"swift::GenericFunctionType::GenericFunctionType(swift::GenericSignature, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>, swift::ASTContext const*, swift::RecursiveTypeProperties)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a {
  case (b: isolated c)
}
