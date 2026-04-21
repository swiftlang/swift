// {"kind":"complete","original":"c7ca60b1","signature":"(anonymous namespace)::TypePrinter::visitAnyFunctionTypeParams(llvm::ArrayRef<swift::AnyFunctionType::Param>, bool, bool, llvm::ArrayRef<swift::LifetimeDependenceInfo>)","signatureAssert":"Assertion failed: (flags.isSending() && \"Only valid when sending is enabled\"), function printParameterFlags","signatureNext":"TypePrinter::visitGenericFunctionType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a<each b>(repeat sending each b)
"\(#^^#)"
