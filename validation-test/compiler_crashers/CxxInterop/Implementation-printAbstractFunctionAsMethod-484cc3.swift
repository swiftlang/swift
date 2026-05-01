// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"15b53d18","signature":"swift::DeclAndTypePrinter::Implementation::printAbstractFunctionAsMethod(swift::AbstractFunctionDecl*, bool, bool, swift::SubscriptDecl const*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"DeclAndTypePrinter::Implementation::visitVarDecl"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
class a {
  var
    b,
    var c: String {
      {
      }
    }
}
