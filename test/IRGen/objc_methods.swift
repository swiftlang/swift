// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

// CHECK: [[REF:%swift.refcounted]] = type
// CHECK: [[FOO:%C12objc_methods3Foo]] = type { [[REF]] }

class Foo {
	func bar() {}     
	func [objc] baz() {}
	func [ibaction] garply() {}
}

// CHECK: @_INSTANCE_METHODS_Foo = private constant { {{.*}}] } {
// CHECK:   i32 24,
// CHECK:   i32 2,
// CHECK:   [2 x { i8*, i8*, i8* }] [{
// CHECK:     i8* getelementptr inbounds ([4 x i8]* @"\01L_selector_data(baz)", i64 0, i64 0),
// CHECK:     i8* null,
// CHECK:     i8* bitcast (void ([[FOO]]*, i8*)* @_TToC12objc_methods3Foo3bazfS0_FT_T_ to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([7 x i8]* @"\01L_selector_data(garply)", i64 0, i64 0),
// CHECK:     i8* null,
// CHECK:     i8* bitcast (void ([[FOO]]*, i8*)* @_TToC12objc_methods3Foo6garplyfS0_FT_T_ to i8*)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8

