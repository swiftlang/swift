// {"kind":"complete","original":"30e899e9","signature":"(anonymous namespace)::TypePrinter::visitAnyFunctionTypeParams(llvm::ArrayRef<swift::AnyFunctionType::Param>, bool, bool, llvm::ArrayRef<swift::LifetimeDependenceInfo>)","signatureAssert":"Assertion failed: (flags.isSending() && \"Only valid when sending is enabled\"), function printParameterFlags","signatureNext":"AnyFunctionType::printParams"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
actor a {
  var collection =
    #^^#
  func archive(b: (sending c))
}
