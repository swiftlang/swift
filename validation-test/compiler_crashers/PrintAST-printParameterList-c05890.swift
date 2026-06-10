// {"kind":"typecheck","original":"3e41ed62","signature":"(anonymous namespace)::PrintAST::printParameterList(swift::ParameterList*, llvm::ArrayRef<swift::AnyFunctionType::Param>, bool, bool)","signatureAssert":"Assertion failed: (flags.isSending() && \"Only valid when sending is enabled\"), function printParameterFlags","signatureNext":"PrintAST::printFunctionParameters"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a
  protocol b {
    c(e: ( sending a
    class d : b
