// RUN: %target-typecheck-verify-swift

// Uses of a deprecated subscript(dynamicMember:) that are synthesized as key path components are diagnosed at the
// member access.

struct DMLScopeA { var a: Int { 0 } }

@dynamicMemberLookup
struct DMLDeprecatedInner {
  @available(*, deprecated, message: "inner")
  subscript<T>(dynamicMember kp: KeyPath<DMLScopeA, T>) -> T { fatalError() }
}

@dynamicMemberLookup
struct DMLOuter {
  subscript<T>(dynamicMember kp: KeyPath<DMLDeprecatedInner, T>) -> T { fatalError() }
}

func testDeprecatedChainedDynamicMember(o: DMLOuter) {
  _ = o.a // expected-warning@:8 {{'subscript(dynamicMember:)' is deprecated: inner}}
  _ = \DMLOuter.a // expected-warning@:17 {{'subscript(dynamicMember:)' is deprecated: inner}}
}

func testDeprecatedDynamicMemberKeyPathLiteral() {
  _ = \DMLDeprecatedInner.a // expected-warning@:27 {{'subscript(dynamicMember:)' is deprecated: inner}}
}
