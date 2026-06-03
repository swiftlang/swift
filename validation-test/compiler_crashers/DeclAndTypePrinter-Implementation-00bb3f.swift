// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"1b99e5c0","signature":"swift::TypeVisitor<swift::DeclAndTypePrinter::Implementation, void, std::__1::optional<swift::OptionalTypeKind>>::visit(swift::Type, std::__1::optional<swift::OptionalTypeKind>)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"DeclAndTypePrinter::Implementation::print"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -emit-clang-header-path /dev/null %s
@_cdecl("") enum a {
}
