// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foo -F %S/Inputs/mock-sdk -module-cache-path=%t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=CHECK_FOO -strict-whitespace < %t.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=FooHelper -F %S/Inputs/mock-sdk -module-cache-path=%t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=CHECK_FOO_HELPER -strict-whitespace < %t.printed.txt

// CHECK_FOO: typealias Class = DynamicLookup.metatype
// CHECK_FOO-NEXT: struct FooSubEnum1 {
// CHECK_FOO-NEXT:   init(value: CUnsignedInt)
// CHECK_FOO-NEXT:   var value: CUnsignedInt
// CHECK_FOO-NEXT: }
// CHECK_FOO-NEXT: var FooSubEnum1X: FooSubEnum1
// CHECK_FOO-NEXT: var FooSubEnum1Y: FooSubEnum1
// CHECK_FOO-NEXT: var FooSubUnnamedEnumeratorA1: CUnsignedInt
// CHECK_FOO-NEXT: typealias __builtin_va_list = (__va_list_tag)
// CHECK_FOO-NEXT: var __swift: CInt
// CHECK_FOO-NEXT: func fooFrameworkFunc1(a: CInt) -> CInt
// CHECK_FOO-NEXT: func fooSubFrameworkFunc1(a: CInt) -> CInt
// CHECK_FOO-NEXT: typealias id = DynamicLookup

// CHECK_FOO_HELPER: typealias Class = DynamicLookup.metatype
// CHECK_FOO_HELPER-NEXT: struct FooHelperSubEnum1 {
// CHECK_FOO_HELPER-NEXT:   init(value: CUnsignedInt)
// CHECK_FOO_HELPER-NEXT:   var value: CUnsignedInt
// CHECK_FOO_HELPER-NEXT: }
// CHECK_FOO_HELPER-NEXT: var FooHelperSubEnum1X: FooHelperSubEnum1
// CHECK_FOO_HELPER-NEXT: var FooHelperSubEnum1Y: FooHelperSubEnum1
// CHECK_FOO_HELPER-NEXT: var FooHelperSubUnnamedEnumeratorA1: CUnsignedInt
// CHECK_FOO_HELPER-NEXT: typealias __builtin_va_list = (__va_list_tag)
// CHECK_FOO_HELPER-NEXT: var __swift: CInt
// CHECK_FOO_HELPER-NEXT: func fooHelperFrameworkFunc1(a: CInt) -> CInt
// CHECK_FOO_HELPER-NEXT: func fooHelperSubFrameworkFunc1(a: CInt) -> CInt
// CHECK_FOO_HELPER-NEXT: typealias id = DynamicLookup

