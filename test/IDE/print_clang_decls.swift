// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=FooClangModule -I %S/Inputs/custom-modules -module-cache-path=%t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_0100 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_0200 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_0300 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_0400 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_0500 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_0600 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_ONE_LINE -strict-whitespace < %t.printed.txt

// PASS_ONE_LINE-DAG: var __swift: CInt

// PASS_ONE_LINE-DAG: typealias id = DynamicLookup
// PASS_ONE_LINE-DAG: typealias Class = DynamicLookup.metatype
// PASS_ONE_LINE-DAG: typealias __builtin_va_list = (__va_list_tag)

// PASS_0100: struct FooEnum1 {
// PASS_0100-NEXT:   init(value: CUnsignedInt)
// PASS_0100-NEXT:   var value: CUnsignedInt
// PASS_0100-NEXT: }
// PASS_ONE_LINE-DAG: var FooEnum1X: FooEnum1

// PASS_0200: struct FooEnum2 {
// PASS_0200-NEXT:   init(value: CUnsignedInt)
// PASS_0200-NEXT:   var value: CUnsignedInt
// PASS_0200-NEXT: }
// PASS_ONE_LINE-DAG: var FooEnum2X: FooEnum2
// PASS_ONE_LINE-DAG: var FooEnum2Y: FooEnum2

// PASS_0300: struct FooEnum3 {
// PASS_0300-NEXT:   init(value: CUnsignedInt)
// PASS_0300-NEXT:   var value: CUnsignedInt
// PASS_0300-NEXT: }
// PASS_ONE_LINE-DAG: var FooEnum3X: FooEnum3
// PASS_ONE_LINE-DAG: var FooEnum3Y: FooEnum3

// PASS_0400: struct FooStruct1 {
// PASS_0400-NEXT:   var x: CInt
// PASS_0400-NEXT:   var y: CDouble
// PASS_0400-NEXT: }

// PASS_0500: struct FooStruct2 {
// PASS_0500-NEXT:   var x: CInt
// PASS_0500-NEXT:   var y: CDouble
// PASS_0500-NEXT: }

// PASS_ONE_LINE-DAG: typealias FooStructTypedef1 = FooStruct2

// PASS_ONE_LINE-DAG: typealias FooStructTypedef2 = FooStructTypedef2

// PASS_ONE_LINE-DAG: typealias FooTypedef1 = CInt

// PASS_ONE_LINE-DAG: var fooIntVar: CInt

// PASS_ONE_LINE-DAG: def fooFunc1(a: CInt) -> CInt
// PASS_ONE_LINE-DAG: def fooFunc1AnonymousParam(_: CInt) -> CInt
// PASS_ONE_LINE-DAG: def fooFunc3(a: CInt, b: CFloat, c: CDouble, d: UnsafePointer<CInt>) -> CInt

// PASS_ONE_LINE-DAG: def redeclaredInMultipleModulesFunc1(a: CInt) -> CInt

// PASS_0600: @objc class FooClass {
// PASS_0600-NEXT:   var fooIntIvar: CInt
// PASS_0600-NEXT:   /* @objc(inferred) */ var fooProperty: CInt
// PASS_0600-NEXT:   @objc def fooInstanceFunc0()
// PASS_0600-NEXT:   @objc def fooInstanceFunc1(a: CInt)
// PASS_0600-NEXT:   @objc def fooInstanceFunc2(a: CInt) withB(b: CInt)
// PASS_0600-NEXT:   @objc static def fooClassFunc0()
// PASS_0600-NEXT: }

