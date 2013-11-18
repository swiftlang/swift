// RUN: %swift -emit-lazy-global-initializers -parse-as-library -emit-llvm -triple x86_64-apple-darwin10 %s | FileCheck %s

// CHECK: @_T12lazy_globals1xSi = global %Si zeroinitializer, align 8
// CHECK: @_T12lazy_globals1ySi = global %Si zeroinitializer, align 8
// CHECK: @_T12lazy_globals1zSi = global %Si zeroinitializer, align 8
// CHECK: @globalinit_token0 = internal global i64 0, align 8

// CHECK: define internal void @globalinit_func0() {
// CHECK: entry:
// CHECK:   store i64 1, i64* getelementptr inbounds (%Si* @_T12lazy_globals1xSi, i32 0, i32 0), align 8
// CHECK:   store i64 2, i64* getelementptr inbounds (%Si* @_T12lazy_globals1ySi, i32 0, i32 0), align 8
// CHECK:   store i64 3, i64* getelementptr inbounds (%Si* @_T12lazy_globals1zSi, i32 0, i32 0), align 8
// CHECK:   ret void
// CHECK: }

// CHECK: define i8* @_T12lazy_globals1xSia() {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_token0, i8* bitcast (void ()* @globalinit_func0 to i8*), %swift.refcounted* null)
// CHECK:   ret i8* bitcast (%Si* @_T12lazy_globals1xSi to i8*)
// CHECK: }

// CHECK: define i8* @_T12lazy_globals1ySia() {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_token0, i8* bitcast (void ()* @globalinit_func0 to i8*), %swift.refcounted* null)
// CHECK:   ret i8* bitcast (%Si* @_T12lazy_globals1ySi to i8*)
// CHECK: }

// CHECK: define i8* @_T12lazy_globals1zSia() {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_token0, i8* bitcast (void ()* @globalinit_func0 to i8*), %swift.refcounted* null)
// CHECK:   ret i8* bitcast (%Si* @_T12lazy_globals1zSi to i8*)
// CHECK: }
var (x, y, z) = (1, 2, 3)

// CHECK: define i64 @_T12lazy_globals4getXFT_Si() {
// CHECK: entry:
// CHECK:   %0 = call i8* @_T12lazy_globals1xSia()
// CHECK:   %1 = bitcast i8* %0 to %Si*
// CHECK:   %.value = getelementptr inbounds %Si* %1, i32 0, i32 0
// CHECK:   %2 = load i64* %.value, align 8
// CHECK:   ret i64 %2
// CHECK: }
func getX() -> Int { return x }

