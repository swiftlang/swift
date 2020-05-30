// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// CHECK-LABEL: @_swift_tupleEquatable_conf = external global %swift.protocol_conformance_descriptor

struct Wrapper<T> {
  let value: T
}

extension Wrapper: Equatable where T: Equatable {}

public func equals<T: Equatable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs == rhs
}

public func use<T: Equatable>(_ thing: T) -> Bool {
  // CHECK: [[USE_WT:%.*]] = call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* @_swift_tupleEquatable_conf, %swift.type* {{%.*}}, i8*** {{%.*}})
  // CHECK-NEXT: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture {{%.*}}, %swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[USE_WT]])
  equals((thing, thing), (thing, thing))
}

public func test() {
  // CHECK: [[TEST_WT1:%.*]] = call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* @_swift_tupleEquatable_conf, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8*** undef)
  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture undef, %swift.opaque* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** [[TEST_WT1]])
  let _ = equals((), ())

  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}({{%.*}}.0* noalias nocapture undef, {{%.*}}.0* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** [[TEST_WT1]])
  let _ = Wrapper(value: ()) == Wrapper(value: ())
}
