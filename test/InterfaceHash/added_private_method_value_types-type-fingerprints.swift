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

// RUN: not diff %t/{a,b}-processed.swiftdeps >%t/diffs

// BEGIN a.swift
struct A {
  func f2() -> Int {
    return 0
  }
}

enum B {
  case x, y
  func f2() -> Int {
    return 0
  }
}

// BEGIN b.swift
struct A {
  func f2() -> Int {
    return 0
  }

  private func f3() -> Int {
    return 1
  }
}

enum B {
  case x, y
  func f2() -> Int {
    return 0
  }

  private func f3() -> Int {
    return 1
  }
}

// RUN: %FileCheck %s <%t/diffs -check-prefix=CHECK-SAME-INTERFACE-HASH
// RUN: %FileCheck %s <%t/diffs -check-prefix=CHECK-DIFFERENT-TYPE-FINGERPRINT

// CHECK-SAME-INTERFACE-HASH-NOT: sourceFileProvides

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < topLevel implementation '' A true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < topLevel interface      '' A true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > topLevel implementation '' A true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > topLevel interface      '' A true

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < nominal implementation 4main1A{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < nominal interface      4main1A{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > nominal implementation 4main1A{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > nominal interface      4main1A{{[^ ]+}} '' true

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < potentialMember implementation 4main1A{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < potentialMember interface      4main1A{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > potentialMember implementation 4main1A{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > potentialMember interface      4main1A{{[^ ]+}} '' true

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < topLevel implementation '' B true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < topLevel interface      '' B true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > topLevel implementation '' B true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > topLevel interface      '' B true

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < nominal implementation 4main1B{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < nominal interface      4main1B{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > nominal implementation 4main1B{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > nominal interface      4main1B{{[^ ]+}} '' true

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < potentialMember implementation 4main1B{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: < potentialMember interface      4main1B{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > potentialMember implementation 4main1B{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: > potentialMember interface      4main1B{{[^ ]+}} '' true
