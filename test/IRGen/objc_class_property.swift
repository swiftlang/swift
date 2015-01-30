// RUN: %target-swift-frontend %s -emit-ir -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// Class properties get reflected to ObjC as class methods. ObjC does not have
// class properties, so no ObjC property is reflected.

// CHECK-NOT: @_PROPERTIES__TtC19objc_class_property7Smashed
// CHECK: @_CLASS_METHODS__TtC19objc_class_property7Smashed = private constant { i32, i32, [1 x { i8*, i8*, i8* }] } {
// CHECK:   i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(sharedSmashed)"
// CHECK-NOT: @_PROPERTIES__TtC19objc_class_property7Smashed
// CHECK: @_INSTANCE_METHODS__TtC19objc_class_property7Smashed = private constant { i32, i32, [1 x { i8*, i8*, i8* }] } {
// CHECK:   i8* getelementptr inbounds ([5 x i8]* @"\01L_selector_data(init)"
// CHECK-NOT: @_PROPERTIES__TtC19objc_class_property7Smashed

@objc class Smashed {
  class var sharedSmashed: Smashed {
    return Smashed()
  }
}

let s = Smashed.sharedSmashed
