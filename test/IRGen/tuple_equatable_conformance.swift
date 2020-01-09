// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// CHECK-LABEL: @"$sxd_tSQsWB" = external global i8*

struct Wrapper<T> {
  let value: T
}

extension Wrapper: Equatable where T: Equatable {}

// CHECK-LABEL: define swiftcc i1 @"$s27tuple_equatable_conformance6equalsySbx_xtSQRzlF"(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.Equatable)
public func equals<T: Equatable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs == rhs
}

// CHECK-LABEL: define swiftcc i1 @"$s27tuple_equatable_conformance3useySbxSQRzlF"(%swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.Equatable)
public func use<T: Equatable>(_ thing: T) -> Bool {
  // CHECK: {{%.*}} = call swiftcc %swift.metadata_response @swift_getTupleTypeMetadata2(i64 0, %swift.type* %T, %swift.type* %T, i8* null, i8** null)

  // CHECK: [[GENERIC_TUPLE_RESPONSE:%.*]] = call swiftcc %swift.metadata_response @swift_getTupleTypeMetadata2(i64 0, %swift.type* %T, %swift.type* %T, i8* null, i8** null)
  // CHECK-NEXT: [[GENERIC_TUPLE_TYPE:%.*]] = extractvalue %swift.metadata_response [[GENERIC_TUPLE_RESPONSE]], 0
  // CHECK-NEXT: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture {{%.*}}, %swift.opaque* noalias nocapture {{%.*}}, %swift.type* [[GENERIC_TUPLE_TYPE]], i8** @"$sxd_tSQsWB")

  equals((thing, thing), (thing, thing))
}

public func test() {
  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture undef, %swift.opaque* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** @"$sxd_tSQsWB")

  let _ = equals((), ())

  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}({{%.*}}.0* noalias nocapture undef, {{%.*}}.0* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** @"$sxd_tSQsWB")

  let _ = Wrapper(value: ()) == Wrapper(value: ())
}
