// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -parse-as-library -emit-ir -primary-file %s | %FileCheck %s

// REQUIRES: CPU=x86_64

// CHECK: @globalinit_[[T:.*]]_token0 = internal global i64 0, align 8
// CHECK: @"$s12lazy_globals1xSivp" = hidden global %TSi zeroinitializer, align 8
// CHECK: @"$s12lazy_globals1ySivp" = hidden global %TSi zeroinitializer, align 8
// CHECK: @"$s12lazy_globals1zSivp" = hidden global %TSi zeroinitializer, align 8

// CHECK: define internal void @globalinit_[[T]]_func0() {{.*}} {
// CHECK: entry:
// CHECK:   store i64 1, i64* getelementptr inbounds (%TSi, %TSi* @"$s12lazy_globals1xSivp", i32 0, i32 0), align 8
// CHECK:   store i64 2, i64* getelementptr inbounds (%TSi, %TSi* @"$s12lazy_globals1ySivp", i32 0, i32 0), align 8
// CHECK:   store i64 3, i64* getelementptr inbounds (%TSi, %TSi* @"$s12lazy_globals1zSivp", i32 0, i32 0), align 8
// CHECK:   ret void
// CHECK: }

// CHECK: define hidden swiftcc i8* @"$s12lazy_globals1xSivau"() {{.*}} {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*), i8* undef)
// CHECK:   ret i8* bitcast (%TSi* @"$s12lazy_globals1xSivp" to i8*)
// CHECK: }

// CHECK: define hidden swiftcc i8* @"$s12lazy_globals1ySivau"() {{.*}} {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*), i8* undef)
// CHECK:   ret i8* bitcast (%TSi* @"$s12lazy_globals1ySivp" to i8*)
// CHECK: }

// CHECK: define hidden swiftcc i8* @"$s12lazy_globals1zSivau"() {{.*}} {
// CHECK: entry:
// CHECK:   call void @swift_once(i64* @globalinit_[[T]]_token0, i8* bitcast (void ()* @globalinit_[[T]]_func0 to i8*), i8* undef)
// CHECK:   ret i8* bitcast (%TSi* @"$s12lazy_globals1zSivp" to i8*)
// CHECK: }
var (x, y, z) = (1, 2, 3)

// CHECK: define hidden swiftcc i64 @"$s12lazy_globals4getXSiyF"() {{.*}} {
// CHECK: entry:
// CHECK:   %0 = call swiftcc i8* @"$s12lazy_globals1xSivau"()
// CHECK:   %1 = bitcast i8* %0 to %TSi*
// CHECK:   %._value = getelementptr inbounds %TSi, %TSi* %1, i32 0, i32 0
// CHECK:   [[V:%.*]] = load i64, i64* %._value, align 8
// CHECK:   ret i64 [[V]]
// CHECK: }
func getX() -> Int { return x }

