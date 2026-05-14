// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -O %s | %FileCheck %s
// 
public struct Foo<let count: Int> {
	// CHECK-LABEL: sil{{.*}} @$s{{.*}}3FooV3bar
	// CHECK:         [[PARAM_VAL:%.*]] = type_value $Int for count
	// CHECK:         [[SPEC_INT:%.*]] = integer_literal ${{.*}}, 3
	// CHECK:         [[PARAM_INT:%.*]] = struct_extract [[PARAM_VAL]], #Int._value
	// CHECK:         builtin "cmp_eq_Int64"([[PARAM_INT]], [[SPEC_INT]])
    @specialized(where count == 3)
    public func bar() -> Int {
		return count
    }
}
