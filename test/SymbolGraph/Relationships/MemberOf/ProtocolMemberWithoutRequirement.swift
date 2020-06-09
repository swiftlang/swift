// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ProtocolMemberWithoutRequirement -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ProtocolMemberWithoutRequirement -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ProtocolMemberWithoutRequirement.symbols.json

public protocol P {}

extension P {
  public func foo() {}
}
public protocol Q : P {}

extension Q {
  public func bar() {}
}

// foo is a member of P.
// CHECK-DAG: "kind": "memberOf",{{[[:space:]]*}}"source": "s:32ProtocolMemberWithoutRequirement1PPAAE3fooyyF",{{[[:space:]]*}}"target": "s:32ProtocolMemberWithoutRequirement1PP"

// bar is a member of Q.
// CHECK-DAG: "kind": "memberOf",{{[[:space:]]*}}"source": "s:32ProtocolMemberWithoutRequirement1QPAAE3baryyF",{{[[:space:]]*}}"target": "s:32ProtocolMemberWithoutRequirement1QP"

// foo is not a requirement of P nor a default implementation for any requirement.
// Neither is bar for Q.
// CHECK-NOT: requirementOf
// CHECK-NOT: defaultImplementationOf 

