// When adding a private protocol method, the interface hash should stay the same
// The per-type fingerprint should change

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: cp %t/{a,x}.swift
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-interface %target-swift-frontend -typecheck -primary-file %t/x.swift -emit-reference-dependencies-path %t/x.swiftdeps -module-name main -module-cache-path %t/ModuleCache -Rmodule-loading
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.py %swift-dependency-tool %t/x.swiftdeps > %t/a-processed.swiftdeps
// RUN: cp %t/{b,x}.swift
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-interface %target-swift-frontend -typecheck -primary-file %t/x.swift -emit-reference-dependencies-path %t/x.swiftdeps -module-name main -module-cache-path %t/ModuleCache -Rmodule-loading
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.py %swift-dependency-tool %t/x.swiftdeps > %t/b-processed.swiftdeps

// RUN: diff %t/a-processed.swiftdeps %t/b-processed.swiftdeps

// BEGIN a.swift
class C {
  func f2() -> Int {
    return 0
  }
}

// BEGIN b.swift
class C {
  func f2() -> Int {
    return 1
  }
}
