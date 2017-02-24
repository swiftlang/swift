// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %S/Inputs/reference-dependencies-members-helper.swift -emit-reference-dependencies-path - > %t.swiftdeps

// Check that the output is deterministic.
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %S/Inputs/reference-dependencies-members-helper.swift -emit-reference-dependencies-path - > %t-2.swiftdeps
// RUN: diff %t.swiftdeps %t-2.swiftdeps

// RUN: %FileCheck -check-prefix=PROVIDES-NOMINAL %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=PROVIDES-NOMINAL-NEGATIVE %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=PROVIDES-MEMBER %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=PROVIDES-MEMBER-NEGATIVE %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=DEPENDS-NOMINAL %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=DEPENDS-NOMINAL-NEGATIVE %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=DEPENDS-MEMBER %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=DEPENDS-MEMBER-NEGATIVE %s < %t.swiftdeps


// PROVIDES-NOMINAL-LABEL: {{^provides-nominal:$}}
// PROVIDES-NOMINAL-NEGATIVE-LABEL: {{^provides-nominal:$}}
// PROVIDES-MEMBER-LABEL: {{^provides-member:$}}
// PROVIDES-MEMBER-NEGATIVE-LABEL: {{^provides-member:$}}
// DEPENDS-NOMINAL-LABEL: {{^depends-nominal:$}}
// DEPENDS-NOMINAL-NEGATIVE-LABEL: {{^depends-nominal:$}}
// DEPENDS-MEMBER-LABEL: {{^depends-member:$}}
// DEPENDS-MEMBER-NEGATIVE-LABEL: {{^depends-member:$}}

// PROVIDES-NOMINAL-DAG: 4BaseC"
class Base {
  // PROVIDES-MEMBER-DAG: - ["{{.+}}4BaseC", ""]
  // PROVIDES-MEMBER-NEGATIVE-NOT: - ["{{.+}}4BaseC", "{{.+}}"]
  func foo() {}
}
  
// PROVIDES-NOMINAL-DAG: 3SubC"
// DEPENDS-NOMINAL-DAG: 9OtherBaseC"
class Sub : OtherBase {
  // PROVIDES-MEMBER-DAG: - ["{{.+}}3SubC", ""]
  // PROVIDES-MEMBER-NEGATIVE-NOT: - ["{{.+}}3SubC", "{{.+}}"]
  // DEPENDS-MEMBER-DAG: - ["{{.+}}9OtherBaseC", ""]
  // DEPENDS-MEMBER-DAG: - ["{{.+}}9OtherBaseC", "foo"]
  // DEPENDS-MEMBER-DAG: - ["{{.+}}9OtherBaseC", "init"]
  func foo() {}
}

// PROVIDES-NOMINAL-DAG: 9SomeProtoP"
// PROVIDES-MEMBER-DAG: - ["{{.+}}9SomeProtoP", ""]
protocol SomeProto {}

// PROVIDES-NOMINAL-DAG: 10OtherClassC"
// PROVIDES-MEMBER-DAG: - ["{{.+}}10OtherClassC", ""]
// DEPENDS-NOMINAL-DAG: 10OtherClassC"
// DEPENDS-NOMINAL-DAG: 9SomeProtoP"
// DEPENDS-MEMBER-DAG: - ["{{.+}}9SomeProtoP", ""]
// DEPENDS-MEMBER-DAG: - ["{{.+}}10OtherClassC", "deinit"]
extension OtherClass : SomeProto {}

// PROVIDES-NOMINAL-NEGATIVE-NOT: 11OtherStructV"{{$}}
// DEPENDS-NOMINAL-DAG: 11OtherStructV"
extension OtherStruct {
  // PROVIDES-MEMBER-DAG: - ["{{.+}}11OtherStructV", ""]
  // PROVIDES-MEMBER-DAG: - ["{{.+}}11OtherStructV", "foo"]
  // PROVIDES-MEMBER-DAG: - ["{{.+}}11OtherStructV", "bar"]
  // PROVIDES-MEMBER-NEGATIVE-NOT: "baz"
  // DEPENDS-MEMBER-DAG: - ["{{.+}}11OtherStructV", "foo"]
  // DEPENDS-MEMBER-DAG: - ["{{.+}}11OtherStructV", "bar"]
  // DEPENDS-MEMBER-DAG: - !private ["{{.+}}11OtherStructV", "baz"]
  // DEPENDS-MEMBER-NEGATIVE-NOT: - ["{{.+}}11OtherStructV", ""]
  func foo() {}
  var bar: () { return () }
  private func baz() {}
}

// PROVIDES-NOMINAL-NEGATIVE-LABEL: {{^depends-nominal:$}}
// PROVIDES-MEMBER-NEGATIVE-LABEL: {{^depends-member:$}}
