// {"kind":"complete","original":"7969fecf","signature":"swift::FunctionType::get(llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType","signatureNext":"AnyFunctionType::withExtInfo"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@MainActor class a {
    b(isolated async )
  {
    {
      #^^#
      let c a  b
