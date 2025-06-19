// {"signature":"swift::FunctionType::get(llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b : FixedWidthInteger extension a : Sequence {
  c {
    { for
        d self { b(truncatingIfNeeded : d
