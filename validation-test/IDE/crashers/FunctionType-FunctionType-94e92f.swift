// {"kind":"complete","original":"7969fecf","signature":"swift::FunctionType::FunctionType(llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Yield>, swift::Type, std::__1::optional<swift::ASTExtInfo>, swift::ASTContext const*, swift::RecursiveTypeProperties)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType","signatureNext":"FunctionType::get"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@MainActor class a {
    b(isolated async )
  {
    {
      #^^#
      let c a  b
