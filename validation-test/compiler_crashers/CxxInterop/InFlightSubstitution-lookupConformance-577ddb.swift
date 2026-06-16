// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"001bd4a5","signature":"swift::InFlightSubstitution::lookupConformance(swift::Type, swift::ProtocolDecl*, unsigned int)","stackOverflow":true}
// This test crashes by overflowing the stack, set a suitable timeout to ensure it doesn't take too long.
// RUN: not %{python} %swift_src_root/test/Inputs/timeout.py 60 \
// RUN:             %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s || \
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a<b: Hashable> {
  case (a<[b]>)
}
