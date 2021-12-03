// RUN: %empty-directory(%t)

// RUN: echo "public let a0 = 0"  >%t/TransitiveDependencies.swift

// RUN: %target-build-swift %t/TransitiveDependencies.swift -emit-module -emit-module-path %t/TransitiveDependencies.swiftmodule -F %t/transitive/framework/search/path -I %t/transitive/normal/search/path
// RUN: %target-build-swift -emit-executable %s -g -o %t/SearchPathParty -emit-module -I %t -F %t/framework/search/path -I %t/normal/search/path
// RUN: %lldb-moduleimport-test -verbose %t/SearchPathParty | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_tools_extra

// Make sure the way LLDB loads modules can handle loading transitive
// dependencies and (in particular) their embedded search paths into the AST
// context. However, we do not want to explicitly propagate those paths into
// this module, hence the negative checks below. N.B. Framework search paths
// are explicitly placed before normal search paths by swift module
// serialization.
import TransitiveDependencies

// CHECK-LABEL: - Serialized search paths:
// CHECK: {{.*}}/framework/search/path (framework)
// CHECK-NOT: {{.*}}/transitive/framework/search/path (framework)
// CHECK: {{.*}}/normal/search/path
// CHECK-NOT: {{.*}}/transitive/normal/search/path
// CHECK: Importing SearchPathParty... ok!
