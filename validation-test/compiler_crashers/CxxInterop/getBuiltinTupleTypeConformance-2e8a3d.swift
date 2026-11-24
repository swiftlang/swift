// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"0300d7c0","signature":"getBuiltinTupleTypeConformance(swift::Type, swift::TupleType const*, swift::ProtocolDecl*)","stackOverflow":true}
// This test crashes by overflowing the stack, set a suitable timeout to ensure it doesn't take too long.
//
// UNSUPPORTED: LinuxDistribution=ubuntu-26.04
//
// RUN: not %{python} %swift_src_root/test/Inputs/timeout.py 30 \
// RUN:             %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s || \
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a<each b> {
  var c: a<repeat (each b, Int)>
}
