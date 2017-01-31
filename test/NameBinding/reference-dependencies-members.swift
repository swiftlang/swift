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

// PROVIDES-NOMINAL-DAG: 4Base"
class Base {
  // PROVIDES-MEMBER-DAG: - ["{{.+}}4Base", ""]
  // PROVIDES-MEMBER-NEGATIVE-NOT: - ["{{.+}}4Base", "{{.+}}"]
  func foo() {}
}
  
// PROVIDES-NOMINAL-DAG: 3Sub"
// DEPENDS-NOMINAL-DAG: 9OtherBase"
class Sub : OtherBase {
  // PROVIDES-MEMBER-DAG: - ["{{.+}}3Sub", ""]
  // PROVIDES-MEMBER-NEGATIVE-NOT: - ["{{.+}}3Sub", "{{.+}}"]
  // DEPENDS-MEMBER-DAG: - ["{{.+}}9OtherBase", ""]
  // DEPENDS-MEMBER-DAG: - ["{{.+}}9OtherBase", "foo"]
  // DEPENDS-MEMBER-DAG: - ["{{.+}}9OtherBase", "init"]
  func foo() {}
}

// PROVIDES-NOMINAL-DAG: 9SomeProto"
// PROVIDES-MEMBER-DAG: - ["{{.+}}9SomeProto", ""]
protocol SomeProto {}

// PROVIDES-NOMINAL-DAG: 10OtherClass"
// PROVIDES-MEMBER-DAG: - ["{{.+}}10OtherClass", ""]
// DEPENDS-NOMINAL-DAG: 10OtherClass"
// DEPENDS-NOMINAL-DAG: 9SomeProto"
// DEPENDS-MEMBER-DAG: - ["{{.+}}9SomeProto", ""]
// DEPENDS-MEMBER-DAG: - ["{{.+}}10OtherClass", "deinit"]
extension OtherClass : SomeProto {}

// PROVIDES-NOMINAL-NEGATIVE-NOT: 11OtherStruct"{{$}}
// DEPENDS-NOMINAL-DAG: 11OtherStruct"
extension OtherStruct {
  // PROVIDES-MEMBER-DAG: - ["{{.+}}11OtherStruct", ""]
  // PROVIDES-MEMBER-DAG: - ["{{.+}}11OtherStruct", "foo"]
  // PROVIDES-MEMBER-DAG: - ["{{.+}}11OtherStruct", "bar"]
  // PROVIDES-MEMBER-NEGATIVE-NOT: "baz"
  // DEPENDS-MEMBER-DAG: - ["{{.+}}11OtherStruct", "foo"]
  // DEPENDS-MEMBER-DAG: - ["{{.+}}11OtherStruct", "bar"]
  // DEPENDS-MEMBER-DAG: - !private ["{{.+}}11OtherStruct", "baz"]
  // DEPENDS-MEMBER-NEGATIVE-NOT: - ["{{.+}}11OtherStruct", ""]
  func foo() {}
  var bar: () { return () }
  private func baz() {}
}

// PROVIDES-NOMINAL-NEGATIVE-LABEL: {{^depends-nominal:$}}
// PROVIDES-MEMBER-NEGATIVE-LABEL: {{^depends-member:$}}
