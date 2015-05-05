// RUN: %target-swift-frontend -parse-as-library -emit-ir -primary-file %s | FileCheck %s

// REQUIRES: CPU=x86_64

// CHECK: @globalinit_[[T:.*]]_token0 = internal global i64 0, align 8
// CHECK: @_Tv12lazy_globals1xSi = global %Si zeroinitializer, align 8
// CHECK: @_Tv12lazy_globals1ySi = global %Si zeroinitializer, align 8
// CHECK: @_Tv12lazy_globals1zSi = global %Si zeroinitializer, align 8

// CHECK: define internal void @globalinit_[[T]]_func0() {
// CHECK: entry:
// CHECK:   store i64 1, i64* getelementptr inbounds (%Si, %Si* @_Tv12lazy_globals1xSi, i32 0, i32 0), align 8
// CHECK:   store i64 2, i64* getelementptr inbounds (%Si, %Si* @_Tv12lazy_globals1ySi, i32 0, i32 0), align 8
// CHECK:   store i64 3, i64* getelementptr inbounds (%Si, %Si* @_Tv12lazy_globals1zSi, i32 0, i32 0), align 8
// CHECK:   ret void
// CHECK: }

// CHECK: define hidden i8* @_TF12lazy_globalsau1xSi() {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*))
// CHECK:   ret i8* bitcast (%Si* @_Tv12lazy_globals1xSi to i8*)
// CHECK: }

// CHECK: define hidden i8* @_TF12lazy_globalsau1ySi() {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*))
// CHECK:   ret i8* bitcast (%Si* @_Tv12lazy_globals1ySi to i8*)
// CHECK: }

// CHECK: define hidden i8* @_TF12lazy_globalsau1zSi() {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*))
// CHECK:   ret i8* bitcast (%Si* @_Tv12lazy_globals1zSi to i8*)
// CHECK: }
var (x, y, z) = (1, 2, 3)

// CHECK: define hidden i64 @_TF12lazy_globals4getXFT_Si() {
// CHECK: entry:
// CHECK:   %0 = call i8* @_TF12lazy_globalsau1xSi()
// CHECK:   %1 = bitcast i8* %0 to %Si*
// CHECK:   %.value = getelementptr inbounds %Si, %Si* %1, i32 0, i32 0
// CHECK:   %2 = load i64, i64* %.value, align 8
// CHECK:   ret i64 %2
// CHECK: }
func getX() -> Int { return x }

