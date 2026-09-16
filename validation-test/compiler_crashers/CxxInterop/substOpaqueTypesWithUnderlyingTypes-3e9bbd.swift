// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"d14aa7cb","signature":"substOpaqueTypesWithUnderlyingTypes","stackOverflow":true}
// This test crashes by overflowing the stack, set a suitable timeout to ensure it doesn't take too long.
// RUN: not %{python} %swift_src_root/test/Inputs/timeout.py 60 \
// RUN:             %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s || \
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s

// This test sometimes still times out on linux, #91374
// UNSUPPORTED: OS=linux-gnu

protocol a
  func b -> some a {
    struct c<d, e>: a {
      f: d
      let g: e
    }
    return c(f: b()
    g
    : b
