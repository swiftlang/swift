// Constant globals using @section
// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// TODO
// XFAIL: *

// integer literals
@section("mysection") let intLiteral1 = 42 // ok
@section("mysection") let intLiteral2: Int8 = 127 // ok
@section("mysection") let intLiteral3: Int16 = 32767 // ok
@section("mysection") let intLiteral4: Int32 = 2147483647 // ok
@section("mysection") let intLiteral5: Int64 = 9223372036854775807 // ok
@section("mysection") let intLiteral6: UInt = 42 // ok
@section("mysection") let intLiteral7: UInt8 = 255 // ok
@section("mysection") let intLiteral8: UInt16 = 65535 // ok
@section("mysection") let intLiteral9: UInt32 = 4294967295 // ok
@section("mysection") let intLiteral10: UInt64 = 18446744073709551615 // ok

// floating-point literals
@section("mysection") let floatLiteral1: Float = 3.14 // ok
@section("mysection") let floatLiteral2: Double = 2.718 // ok

// boolean literals
@section("mysection") let boolLiteral1 = true // ok
@section("mysection") let boolLiteral2 = false // ok

func foo() -> Int { return 42 }
func bar(x: Int) -> String { return "test" }

// function references
@section("mysection") let funcRef1 = foo // ok
@section("mysection") let funcRef2 = bar // ok

// metatypes - TODO
//@section("mysection") let metatype1 = Int.self

// tuples
@section("mysection") let tuple1 = (1, 2, 3, 2.718, true) // ok
@section("mysection") let tuple2: (Int, Float, Bool) = (42, 3.14, false) // ok
@section("mysection") let tuple3 = (foo, bar) // ok (function references in tuple)

// CHECK: @"$s9SectionIR11intLiteral1Sivp" = {{.*}}constant %TSi <{ i64 42 }>, section "mysection"
// CHECK: @"$s9SectionIR11intLiteral2s4Int8Vvp" = {{.*}}constant %Ts4Int8V <{ i8 127 }>, section "mysection"
// CHECK: @"$s9SectionIR11intLiteral3s5Int16Vvp" = {{.*}}constant %Ts5Int16V <{ i16 32767 }>, section "mysection"
// CHECK: @"$s9SectionIR11intLiteral4s5Int32Vvp" = {{.*}}constant %Ts5Int32V <{ i32 2147483647 }>, section "mysection"
// CHECK: @"$s9SectionIR11intLiteral5s5Int64Vvp" = {{.*}}constant %Ts5Int64V <{ i64 9223372036854775807 }>, section "mysection"
// CHECK: @"$s9SectionIR11intLiteral6Suvp" = {{.*}}constant %TSu <{ i64 42 }>, section "mysection"
// CHECK: @"$s9SectionIR11intLiteral7s5UInt8Vvp" = {{.*}}constant %Ts5UInt8V <{ i8 -1 }>, section "mysection"
// CHECK: @"$s9SectionIR11intLiteral8s6UInt16Vvp" = {{.*}}constant %Ts6UInt16V <{ i16 -1 }>, section "mysection"
// CHECK: @"$s9SectionIR11intLiteral9s6UInt32Vvp" = {{.*}}constant %Ts6UInt32V <{ i32 -1 }>, section "mysection"
// CHECK: @"$s9SectionIR12intLiteral10s6UInt64Vvp" = {{.*}}constant %Ts6UInt64V <{ i64 -1 }>, section "mysection"
// CHECK: @"$s9SectionIR13floatLiteral1Sfvp" = {{.*}}constant %TSf <{ float 0x40091EB860000000 }>, section "mysection"
// CHECK: @"$s9SectionIR13floatLiteral2Sdvp" = {{.*}}constant %TSd <{ double 2.718000e+00 }>, section "mysection"
// CHECK: @"$s9SectionIR12boolLiteral1Sbvp" = {{.*}}constant %TSb <{ i1 true }>, section "mysection"
// CHECK: @"$s9SectionIR12boolLiteral2Sbvp" = {{.*}}constant %TSb zeroinitializer, section "mysection"
// CHECK: @"$s9SectionIR8funcRef1Siycvp" = {{.*}}constant %swift.function { ptr @"$s9SectionIR3fooSiyF", ptr null }, section "mysection"
// CHECK: @"$s9SectionIR8funcRef2ySSSicvp" = {{.*}}constant %swift.function { ptr @"$s9SectionIR3bar1xSSSi_tF", ptr null }, section "mysection"
// CHECK: @"$s9SectionIR6tuple1Si_S2iSdSbtvp" = {{.*}}constant <{ %TSi, %TSi, %TSi, %TSd, %TSb }> <{ %TSi <{ i64 1 }>, %TSi <{ i64 2 }>, %TSi <{ i64 3 }>, %TSd <{ double 2.718000e+00 }>, %TSb <{ i1 true }> }>, section "mysection"
// CHECK: @"$s9SectionIR6tuple2Si_SfSbtvp" = {{.*}}constant <{ %TSi, %TSf, %TSb }> <{ %TSi <{ i64 42 }>, %TSf <{ float 0x40091EB860000000 }>, %TSb zeroinitializer }>, section "mysection"
// CHECK: @"$s9SectionIR6tuple3Siyc_SSSictvp" = {{.*}}constant <{ %swift.function, %swift.function }> <{ %swift.function { ptr @"$s9SectionIR3fooSiyF", ptr null }, %swift.function { ptr @"$s9SectionIR3bar1xSSSi_tF", ptr null } }>, section "mysection"
