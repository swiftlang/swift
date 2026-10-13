// RUN: %swift -swift-version 4 -target arm64e-apple-ios12.0 -parse-stdlib -parse-as-library %s -emit-ir -module-name test -Xcc -Xclang -Xcc -fptrauth-calls | %FileCheck %s --check-prefix=CHECK

// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

@objc class A {
  @objc func foo() {}
}

// CHECK: @"OBJC_METACLASS_$__TtC4test1A" = hidden global %objc_class { ptr [[A_META_ISA:@[^,]+]], ptr [[A_META_SUPER:@[^,]+]], ptr @_objc_empty_cache, ptr null, ptr [[A_META_RO:@[^ ]+]] }
// CHECK: [[A_ISA:@[^ ]+]] = private constant { ptr, i32, i64, i64 } { ptr @"OBJC_METACLASS_$__TtC4test1A", i32 2, i64 ptrtoint (ptr getelementptr inbounds ({{.*}}, ptr @"$s4test1ACMf", i32 0, i32 3) to i64), i64 27361 }, section "llvm.ptrauth"
// CHECK: [[A_SUPER:@[^ ]+]] = private constant { ptr, i32, i64, i64 } { ptr @"OBJC_CLASS_$__TtCs12_SwiftObject", i32 2, i64 ptrtoint (ptr getelementptr inbounds ({{.*}}, ptr @"$s4test1ACMf", i32 0, i32 4) to i64), i64 46507 }, section "llvm.ptrauth"
// CHECK: [[A_META_ISA]] = private constant { ptr, i32, i64, i64 } { ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", i32 2, i64 ptrtoint (ptr @"OBJC_METACLASS_$__TtC4test1A" to i64), i64 27361 }, section "llvm.ptrauth"
// CHECK: [[A_META_SUPER]] = private constant { ptr, i32, i64, i64 } { ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", i32 2, i64 ptrtoint (ptr getelementptr inbounds (%objc_class, ptr @"OBJC_METACLASS_$__TtC4test1A", i32 0, i32 1) to i64), i64 46507 }, section "llvm.ptrauth"
// CHECK: [[A_META_RO]] = private constant { ptr, i32, i64, i64 } { ptr @_METACLASS_DATA__TtC4test1A, i32 2, i64 ptrtoint (ptr getelementptr inbounds (%objc_class, ptr @"OBJC_METACLASS_$__TtC4test1A", i32 0, i32 4) to i64), i64 25080 }, section "llvm.ptrauth"

// CHECK: @"$s4test1AC3fooyyFTo.ptrauth" = private constant { ptr, i32, i64, i64 } { ptr @"$s4test1AC3fooyyFTo", i32 0, i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, [1 x { ptr, ptr, ptr }] }, ptr @_INSTANCE_METHODS__TtC4test1A, i32 0, i32 2, i32 0, i32 2) to i64), i64 0 }, section "llvm.ptrauth"

// CHECK: [[A_RO:@[^ ]+]] = private constant { ptr, i32, i64, i64 } { ptr getelementptr (i8, ptr @_DATA__TtC4test1A, i64 {{1|2}}), i32 2, i64 ptrtoint (ptr getelementptr inbounds ({{.*}}, ptr @"$s4test1ACMf", i32 0, i32 7) to i64), i64 25080 }, section "llvm.ptrauth"
// CHECK: @"$s4test1ACMf" = internal global <{ {{.*}} }> <{ ptr null, ptr {{@[^,]+}}, ptr {{@[^,]+}}, ptr [[A_ISA]], ptr [[A_SUPER]], ptr @_objc_empty_cache, ptr null, ptr [[A_RO]],

struct S {}

// The metaclass in the metadata pattern of a generic class.
class C<T> {}
// CHECK: [[C_PATTERN_ISA:@[^ ]+]] = private constant { ptr, i32, i64, i64 } { ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", i32 2, i64 ptrtoint (ptr [[C_PATTERN:@[0-9]+]] to i64), i64 27361 }, section "llvm.ptrauth"
// CHECK: [[C_PATTERN]] = internal constant <{ {{.*}} }> <{ ptr [[C_PATTERN_ISA]], ptr null, ptr @_objc_empty_cache, ptr null, i64 0,

// The superclass of D's metaclass is filled in at runtime.
class D : C<S> {}
// CHECK: @"$s4test1DCMm" = hidden global %objc_class { ptr {{@[^,]+}}, ptr null, ptr @_objc_empty_cache, ptr null, ptr {{@[^ ]+}} }

@objc protocol P {
  func bar()
}
@objc class B : P {
  func bar() {}
}
// CHECK: @_PROTOCOL_INSTANCE_METHODS__TtP4test1P_ = {{.*}} [{ ptr, ptr, ptr } { ptr @"\01L_selector_data(bar)", ptr {{@.*}}, ptr null }]
