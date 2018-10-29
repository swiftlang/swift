// RUN: rm -rf %t
// RUN: %target-build-swift -index-file -index-file-path %s %s %S/Inputs/SwiftModuleA.swift -module-name unit_one_test -o %t.output_for_index -index-store-path %t/idx
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=UNIT

// UNIT-NOT: SwiftShims

// UNIT: [[SWIFT:Swift.swiftmodule-[A-Z0-9]*]]
// UNIT: DEPEND START
// UNIT: Record | system | Swift.Math.Floating | {{.*}}Swift.swiftmodule | Swift.swiftmodule_Math_Floating-{{.*}}
// UNIT: Record | system | Swift.String | {{.*}}Swift.swiftmodule | Swift.swiftmodule_String-{{.*}}
// UNIT: DEPEND END

// UNIT: unit-one-file-multi-file-invocation{{.*}}.output_for_index
// UNIT: DEPEND START
// UNIT: Unit | system |{{.*}}/Swift.swiftmodule | [[SWIFT]]
// UNIT: Record | user |{{.*}}/unit-one-file-multi-file-invocation.swift |
// UNIT: DEPEND END (2)

func test1() {
  funcSwiftA()
}
