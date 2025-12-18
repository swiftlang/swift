// {"kind":"typecheck","signature":"(anonymous namespace)::TypePrinter::visitAnyFunctionTypeParams(llvm::ArrayRef<swift::AnyFunctionType::Param>, bool, llvm::ArrayRef<swift::LifetimeDependenceInfo>)","signatureAssert":"Assertion failed: (flags.isSending() && \"Only valid when sending is enabled\"), function printParameterFlags"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a->String{ b}
         func b(c : (sending String
