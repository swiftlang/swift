// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -parse-as-library -emit-ir -primary-file %s | %FileCheck %s

// REQUIRES: CPU=x86_64

// CHECK: @globalinit_[[T:.*]]_token0 = internal global i64 0, align 8
// CHECK: @_T012lazy_globals1xSiv = hidden global %TSi zeroinitializer, align 8
// CHECK: @_T012lazy_globals1ySiv = hidden global %TSi zeroinitializer, align 8
// CHECK: @_T012lazy_globals1zSiv = hidden global %TSi zeroinitializer, align 8

// CHECK: define internal swiftcc void @globalinit_[[T]]_func0() {{.*}} {
// CHECK: entry:
// CHECK:   store i64 1, i64* getelementptr inbounds (%TSi, %TSi* @_T012lazy_globals1xSiv, i32 0, i32 0), align 8
// CHECK:   store i64 2, i64* getelementptr inbounds (%TSi, %TSi* @_T012lazy_globals1ySiv, i32 0, i32 0), align 8
// CHECK:   store i64 3, i64* getelementptr inbounds (%TSi, %TSi* @_T012lazy_globals1zSiv, i32 0, i32 0), align 8
// CHECK:   ret void
// CHECK: }

// CHECK: define hidden swiftcc i8* @_T012lazy_globals1xSifau() {{.*}} {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*))
// CHECK:   ret i8* bitcast (%TSi* @_T012lazy_globals1xSiv to i8*)
// CHECK: }

// CHECK: define hidden swiftcc i8* @_T012lazy_globals1ySifau() {{.*}} {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*))
// CHECK:   ret i8* bitcast (%TSi* @_T012lazy_globals1ySiv to i8*)
// CHECK: }

// CHECK: define hidden swiftcc i8* @_T012lazy_globals1zSifau() {{.*}} {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*))
// CHECK:   ret i8* bitcast (%TSi* @_T012lazy_globals1zSiv to i8*)
// CHECK: }
var (x, y, z) = (1, 2, 3)

// CHECK: define hidden swiftcc i64 @_T012lazy_globals4getXSiyF() {{.*}} {
// CHECK: entry:
// CHECK:   %0 = call swiftcc i8* @_T012lazy_globals1xSifau()
// CHECK:   %1 = bitcast i8* %0 to %TSi*
// CHECK:   %._value = getelementptr inbounds %TSi, %TSi* %1, i32 0, i32 0
// CHECK:   %2 = load i64, i64* %._value, align 8
// CHECK:   ret i64 %2
// CHECK: }
func getX() -> Int { return x }

