// REQUIRES: shell
// Also uses awk:
// XFAIL OS=windows

// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Need -fine-grained-dependency-include-intrafile to be invarient wrt type-body-fingerprints enabled/disabled
// RUN: %target-swift-frontend -fine-grained-dependency-include-intrafile -typecheck -primary-file %t/main.swift %S/../Inputs/reference-dependencies-members-helper.swift -disable-direct-intramodule-dependencies -emit-reference-dependencies-path - > %t.swiftdeps

// RUN: %target-swift-frontend -fine-grained-dependency-include-intrafile -typecheck -primary-file %t/main.swift %S/../Inputs/reference-dependencies-members-helper.swift -disable-direct-intramodule-dependencies -emit-reference-dependencies-path - > %t-2.swiftdeps
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t.swiftdeps %t-processed.swiftdeps
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t-2.swiftdeps %t-2-processed.swiftdeps

// RUN: diff %t-processed.swiftdeps %t-2-processed.swiftdeps

// RUN: %FileCheck -check-prefix=PROVIDES-NOMINAL %s < %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=PROVIDES-NOMINAL-2 %s < %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=PROVIDES-MEMBER %s < %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=PROVIDES-MEMBER-NEGATIVE %s < %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=DEPENDS-NOMINAL %s < %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=DEPENDS-MEMBER %s < %t-processed.swiftdeps

// PROVIDES-NOMINAL-DAG:  nominal implementation  4main4BaseC '' true
// PROVIDES-NOMINAL-DAG:  nominal interface  4main4BaseC '' true
class Base {
  // PROVIDES-MEMBER-DAG:  potentialMember implementation  4main4BaseC '' true
  // PROVIDES-MEMBER-DAG:  potentialMember interface  4main4BaseC '' true
  // PROVIDES-MEMBER-NEGATIVE-NOT:  member {{.*}}  4main4BaseC {{[^']]+}} true
  func foo() {}
}
  
// PROVIDES-NOMINAL-DAG:  nominal implementation  4main3SubC '' true
// PROVIDES-NOMINAL-DAG:  nominal interface  4main3SubC '' true
// DEPENDS-NOMINAL-DAG:  nominal interface  4main9OtherBaseC '' false
class Sub : OtherBase {
  // PROVIDES-MEMBER-DAG:  potentialMember implementation  4main3SubC '' true
  // PROVIDES-MEMBER-NEGATIVE-NOT:  {{potentialM|m}}}}ember implementation  4main3SubC {{.+}} true
  // DEPENDS-MEMBER-DAG:  potentialMember interface  4main9OtherBaseC '' false
  // DEPENDS-MEMBER-DAG:  member interface  4main9OtherBaseC foo false
  // DEPENDS-MEMBER-DAG:  member interface  4main9OtherBaseC init false
  func foo() {}
}

// PROVIDES-NOMINAL-DAG:  nominal implementation  4main9SomeProtoP '' true
// PROVIDES-NOMINAL-DAG:  nominal interface  4main9SomeProtoP '' true
// PROVIDES-MEMBER-DAG:  potentialMember interface  4main9SomeProtoP '' true
protocol SomeProto {}

// PROVIDES-NOMINAL-DAG:  nominal implementation  4main10OtherClassC '' true
// PROVIDES-NOMINAL-2-DAG:  nominal interface  4main10OtherClassC '' true
// PROVIDES-MEMBER-DAG:  potentialMember interface  4main10OtherClassC '' true
// DEPENDS-MEMBER-DAG:  potentialMember interface  4main10OtherClassC '' true
extension OtherClass : SomeProto {}

// PROVIDES-NOMINAL-DAG:  nominal implementation  4main11OtherStructV '' true
// PROVIDES-NOMINAL-DAG:  nominal interface  4main11OtherStructV '' true
extension OtherStruct {
  // PROVIDES-MEMBER-DAG:  potentialMember interface  4main11OtherStructV '' true
  // PROVIDES-MEMBER-DAG:  member interface  4main11OtherStructV foo true
  // PROVIDES-MEMBER-DAG:  member interface  4main11OtherStructV bar true
  // PROVIDES-MEMBER-DAG:  member interface  4main11OtherStructV baz true
  // DEPENDS-MEMBER-DAG:  potentialMember interface  4main11OtherStructV '' true
  func foo() {}
  var bar: () { return () }
  private func baz() {}
}
