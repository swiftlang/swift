// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"d14aa7cb","signature":"substOpaqueTypesWithUnderlyingTypes"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a
  func b -> some a {
    struct c<d, e>: a {
      f: d
      let g: e
    }
    return c(f: b()
    g
    : b
