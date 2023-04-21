// REQUIRES: shell
// Also uses awk:
// XFAIL OS=windows

// When adding a private protocol method, the interface hash should stay the same
// The per-type fingerprint should change

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: cp %t/{a,x}.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/x.swift -emit-reference-dependencies-path %t/x.swiftdeps -module-name main
// RUN: %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.sh %swift-dependency-tool %t/x.swiftdeps %t/a-processed.swiftdeps
// RUN: cp %t/{b,x}.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/x.swift -emit-reference-dependencies-path %t/x.swiftdeps -module-name main
// RUN: %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.sh %swift-dependency-tool %t/x.swiftdeps %t/b-processed.swiftdeps

// We can use `diff` here because this test isn't run on Windows
// RUN: diff %t/{a,b}-processed.swiftdeps

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
