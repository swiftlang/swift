// When adding a private protocol method, the interface hash should stay the same
// The per-type fingerprint should change

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: cp %t/{a,x}.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/x.swift -emit-reference-dependencies-path %t/x.swiftdeps -module-name main
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.py %swift-dependency-tool %t/x.swiftdeps > %t/a-processed.swiftdeps
// RUN: cp %t/{b,x}.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/x.swift -emit-reference-dependencies-path %t/x.swiftdeps -module-name main
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.py %swift-dependency-tool %t/x.swiftdeps > %t/b-processed.swiftdeps

// RUN: not diff -u %t/a-processed.swiftdeps %t/b-processed.swiftdeps > %t/diffs

// BEGIN a.swift
private protocol P {
  func f2() -> Int
  var y: Int { get set }
}

// BEGIN b.swift
private protocol P {
  func f2() -> Int
  func f3() -> Int
  var y: Int { get set }
}

// RUN: %FileCheck %s <%t/diffs -check-prefix=CHECK-SAME-INTERFACE-HASH
// RUN: %FileCheck %s <%t/diffs -check-prefix=CHECK-DIFFERENT-TYPE-FINGERPRINT

// CHECK-SAME-INTERFACE-HASH-NOT: sourceFileProvides

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: -topLevel implementation '' P true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: -topLevel interface      '' P true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: +topLevel implementation '' P true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: +topLevel interface      '' P true

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: -nominal implementation 4main1P{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: -nominal interface      4main1P{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: +nominal implementation 4main1P{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: +nominal interface      4main1P{{[^ ]+}} '' true

// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: -potentialMember implementation 4main1P{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: -potentialMember interface      4main1P{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: +potentialMember implementation 4main1P{{[^ ]+}} '' true
// CHECK-DIFFERENT-TYPE-FINGERPRINT-DAG: +potentialMember interface      4main1P{{[^ ]+}} '' true
