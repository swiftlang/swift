// REQUIRES: shell
// Also uses awk:
// XFAIL OS=windows

// When adding a private protocol method, the interface hash should stay the same
// The per-type fingerprint should change

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: cp %t/{a,x}.swift
// RUN: %target-swift-frontend -typecheck -enable-fine-grained-dependencies -enable-type-fingerprints -primary-file %t/x.swift -emit-reference-dependencies-path %t/x.swiftdeps -module-name main
// RUN: %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.sh <%t/x.swiftdeps >%t/a-processed.swiftdeps
// RUN: cp %t/{b,x}.swift
// RUN: %target-swift-frontend -typecheck -enable-fine-grained-dependencies -enable-type-fingerprints -primary-file %t/x.swift -emit-reference-dependencies-path %t/x.swiftdeps -module-name main
// RUN: %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.sh <%t/x.swiftdeps >%t/b-processed.swiftdeps

// RUN: cmp %t/{a,b}-processed.swiftdeps 

// BEGIN a.swift
class C {
  var p: Int {
    return 0
  }
}

// BEGIN b.swift
class C {
  var p: Int {
    let x = 1
    return x
  }
}

