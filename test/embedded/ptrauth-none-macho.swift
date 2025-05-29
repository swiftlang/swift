// RUN: %target-swift-frontend -target arm64e-apple-none-macho -enable-experimental-feature Embedded -emit-ir %s -o - | %FileCheck %s
// RUN: %target-swift-frontend -target arm64e-apple-macos14 -enable-experimental-feature Embedded -emit-ir %s -o - | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

var callback: (()->())? = nil
var callback2: ((Bool)->())? = nil
var callback3: ((Bool, Bool)->())? = nil

public func foo() {
  callback?()
  callback2?(true)
  callback3?(true, true)
}

// CHECK-LABEL: define swiftcc void @"$e4main3fooyyF"()

// CHECK:       [[P1:%.*]] = load ptr, ptr @"$e4main8callbackyycSgvp"
// CHECK:       call swiftcc void [[P1:%.*]](ptr swiftself {{.*}}) [ "ptrauth"(i32 0, i64 3848) ]

// CHECK:       [[P2:%.*]] = load ptr, ptr @"$e4main9callback2ySbcSgvp"
// CHECK:       call swiftcc void [[P2:%.*]](i1 true, ptr swiftself {{.*}}) [ "ptrauth"(i32 0, i64 25457) ]

// CHECK:       [[P3:%.*]] = load ptr, ptr @"$e4main9callback3ySb_SbtcSgvp"
// CHECK:       call swiftcc void [[P3:%.*]](i1 true, i1 true, ptr swiftself {{.*}}) [ "ptrauth"(i32 0, i64 25424) ]
