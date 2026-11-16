// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"359e75a9","signature":"swift::irgen::applyLayoutAttributes(swift::irgen::IRGenModule&, swift::NominalTypeDecl*, bool, swift::irgen::Alignment&)","signatureAssert":"Assertion failed: (value != 0 && ((value - 1) & value) == 0 && \"alignment not a power of two!\"), function applyLayoutAttributes","signatureNext":"StructLayout::StructLayout"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-path /dev/null %s
@_alignment(0) public struct a {
}
