// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// CHECK-LABEL: @_swift_tupleEquatable_wt = external global i8*

struct Wrapper<T> {
  let value: T
}

extension Wrapper: Equatable where T: Equatable {}

public func equals<T: Equatable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs == rhs
}

public func use<T: Equatable>(_ thing: T) -> Bool {
  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture {{%.*}}, %swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** @_swift_tupleEquatable_wt)
  equals((thing, thing), (thing, thing))
}

public func test() {
  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture undef, %swift.opaque* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** @_swift_tupleEquatable_wt)
  let _ = equals((), ())

  // CHECK: {{%.*}} = call swiftcc i1 {{.*}}({{%.*}}.0* noalias nocapture undef, {{%.*}}.0* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** @_swift_tupleEquatable_wt)
  let _ = Wrapper(value: ()) == Wrapper(value: ())
}
