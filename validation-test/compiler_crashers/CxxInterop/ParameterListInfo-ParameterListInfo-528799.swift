// {"kind":"typecheck","signature":"swift::ParameterListInfo::ParameterListInfo(llvm::ArrayRef<swift::AnyFunctionType::Param>, bool, swift::ConcreteDeclRef)","signatureAssert":"Assertion failed: (Index < this->size() && \"Invalid index!\"), function operator[]","signatureNext":"ExprRewriter::coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck -cxx-interoperability-mode=default -import-objc-header %S/Inputs/ParameterListInfo-ParameterListInfo-9bbdcd/header.h %s

func f(s: S) {
  printT(s, 1)
}
