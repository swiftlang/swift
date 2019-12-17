// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// CHECK-LABEL: @"$sytSQsWB" = external global i8*

func equals<T: Equatable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs == rhs
}

// CHECK: {{%.*}} = call swiftcc i1 {{.*}}(%swift.opaque* noalias nocapture undef, %swift.opaque* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** @"$sytSQsWB")

let _ = equals((), ())

struct Wrapper<T> {
  let value: T
}

extension Wrapper: Equatable where T: Equatable {}

// CHECK: {{%.*}} = call swiftcc i1 {{.*}}({{%.*}}.0* noalias nocapture undef, {{%.*}}.0* noalias nocapture undef, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), i8** @"$sytSQsWB")

let _ = Wrapper(value: ()) == Wrapper(value: ())
