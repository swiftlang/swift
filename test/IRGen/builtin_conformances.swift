// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// CHECK-LABEL: @_swift_tupleEquatable_conf = external global %swift.protocol_conformance_descriptor
// CHECK-LABEL: @_swift_tupleComparable_conf = external global %swift.protocol_conformance_descriptor
// CHECK-LABEL: @_swift_tupleHashable_conf = external global %swift.protocol_conformance_descriptor

struct Wrapper<T> {
  let value: T
}

//===----------------------------------------------------------------------===//
// Tuple Equatable conformance
//===----------------------------------------------------------------------===//

extension Wrapper: Equatable where T: Equatable {}

public func equals<T: Equatable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs == rhs
}

public func useEquatable<T: Equatable>(_ thing: T) -> Bool {
  // CHECK: [[USE_EQUATABLE_WT:%.*]] = call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* @_swift_tupleEquatable_conf, %swift.type* {{%.*}}, i8*** {{%.*}})
  // CHECK-NEXT: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture {{%.*}}, %swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[USE_EQUATABLE_WT]])
  equals((thing, thing), (thing, thing))
}

public func testTupleEquatable() {
  // CHECK: [[TEST_TUPLE_EQUATABLE_WT1:%.*]] = call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* @_swift_tupleEquatable_conf, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8*** undef)
  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture undef, %swift.opaque* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** [[TEST_TUPLE_EQUATABLE_WT1]])
  let _ = equals((), ())

  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}({{%.*}}.0* noalias nocapture undef, {{%.*}}.0* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** [[TEST_TUPLE_EQUATABLE_WT1]])
  let _ = Wrapper(value: ()) == Wrapper(value: ())
}

//===----------------------------------------------------------------------===//
// Tuple Comparable conformance
//===----------------------------------------------------------------------===//

extension Wrapper: Comparable where T: Comparable {
  static func <(lhs: Wrapper, rhs: Wrapper) -> Bool {
    return lhs.value < rhs.value
  }
}

public func compare<T: Comparable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs < rhs
}

public func useComparable<T: Comparable>(_ thing: T) -> Bool {
  // CHECK: [[USE_COMPARABLE_WT:%.*]] = call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* @_swift_tupleComparable_conf, %swift.type* {{%.*}}, i8*** {{%.*}})
  // CHECK-NEXT: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture {{%.*}}, %swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[USE_COMPARABLE_WT]])
  compare((thing, thing), (thing, thing))
}

public func testTupleComparable() {
  // CHECK: [[TEST_TUPLE_COMPARABLE_WT1:%.*]] = call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* @_swift_tupleComparable_conf, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8*** undef)
  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture undef, %swift.opaque* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** [[TEST_TUPLE_COMPARABLE_WT1]])
  let _ = compare((), ())

  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}({{%.*}}.1* noalias nocapture undef, {{%.*}}.1* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** [[TEST_TUPLE_COMPARABLE_WT1]])
  let _ = Wrapper(value: ()) < Wrapper(value: ())
}

//===----------------------------------------------------------------------===//
// Tuple Hashable conformance
//===----------------------------------------------------------------------===//

extension Wrapper: Hashable where T: Hashable {}

public func hashValue<T: Hashable>(for instance: T) -> Int {
  instance.hashValue
}

public func useHashable<T: Hashable>(_ thing: T) -> Int {
  // CHECK: [[USE_HASHABLE_WT:%.*]] = call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* @_swift_tupleHashable_conf, %swift.type* {{%.*}}, i8*** {{%.*}})
  // CHECK-NEXT: {{%.*}} = call swiftcc i64 {{.*}}(%swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[USE_HASHABLE_WT]])
  hashValue(for: (thing, thing))
}

public func testTupleHashable() {
  // CHECK: [[TEST_TUPLE_HASHABLE_WT1:%.*]] = call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* @_swift_tupleHashable_conf, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8*** undef)
  // CHECK: {{%.*}} = call swiftcc i64 {{.*}}(%swift.opaque* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** [[TEST_TUPLE_HASHABLE_WT1]])
  let _ = hashValue(for: ())

  // CHECK: {{%.*}} = call swiftcc i64 {{.*}}(%swift.type* {{%.*}}, i8** [[TEST_TUPLE_HASHABLE_WT1]], {{%.*}} noalias nocapture swiftself undef)
  let _ = Wrapper(value: ()).hashValue
}
