// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"000ddb3d","signature":"swift::DeclAndTypePrinter::Implementation::printAbstractFunctionAsMethod(swift::AbstractFunctionDecl*, bool, bool, swift::SubscriptDecl const*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"DeclAndTypePrinter::Implementation::printMembers"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a
  struct b {
    c: a
    subscript    ->()  {
      {
