// {"kind":"typecheck","signature":"swift::FunctionType::get(llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a {
  case (b: isolated c)
}
