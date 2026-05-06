// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"5ffd62cc","signature":"(anonymous namespace)::SubstFunctionTypePatternVisitor::handlePackExpansion(swift::Lowering::AbstractionPattern, swift::CanType)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"SubstFunctionTypePatternVisitor::handleUnabstractedFunctionType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a<each b> {
  case (c: (repeat each b) -> Void)
}
func d<b>() -> a<b>
