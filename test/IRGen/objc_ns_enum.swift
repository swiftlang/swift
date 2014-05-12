// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-darwin10 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s -emit-ir | FileCheck %s

import Foundation
import gizmo

// CHECK: @_TWVOSC16NSRuncingOptions = linkonce_odr hidden constant
// CHECK: @_TMnOSC16NSRuncingOptions = linkonce_odr hidden constant
// CHECK: @_TMdOSC16NSRuncingOptions = linkonce_odr hidden constant

// CHECK: define i16 @_TF12objc_ns_enum22imported_enum_inject_aFT_OSC16NSRuncingOptions()
// CHECK:   ret i16 123
func imported_enum_inject_a() -> NSRuncingOptions {
  return .Mince
}

// CHECK: define i16 @_TF12objc_ns_enum22imported_enum_inject_bFT_OSC16NSRuncingOptions()
// CHECK:   ret i16 4567
func imported_enum_inject_b() -> NSRuncingOptions {
  return .QuinceSliced
}

// CHECK: define i16 @_TF12objc_ns_enum22imported_enum_inject_cFT_OSC16NSRuncingOptions()
// CHECK:   ret i16 5678
func imported_enum_inject_c() -> NSRuncingOptions {
  return .QuinceJulienned
}

// CHECK: define i16 @_TF12objc_ns_enum22imported_enum_inject_dFT_OSC16NSRuncingOptions()
// CHECK:   ret i16 6789
func imported_enum_inject_d() -> NSRuncingOptions {
  return .QuinceDiced
}

// CHECK: define i32 @_TF12objc_ns_enum30imported_enum_inject_radixed_aFT_OSC16NSRadixedOptions() {
// -- octal 0755
// CHECK:   ret i32 493
func imported_enum_inject_radixed_a() -> NSRadixedOptions {
  return .Octal
}

// CHECK: define i32 @_TF12objc_ns_enum30imported_enum_inject_radixed_bFT_OSC16NSRadixedOptions() {
// -- hex 0xFFFF
// CHECK:   ret i32 65535
func imported_enum_inject_radixed_b() -> NSRadixedOptions {
  return .Hex
}

// CHECK: define i32 @_TF12objc_ns_enum31imported_enum_inject_negative_aFT_OSC17NSNegativeOptions() {
// CHECK:   ret i32 -1
func imported_enum_inject_negative_a() -> NSNegativeOptions {
  return .Foo
}

// CHECK: define i32 @_TF12objc_ns_enum31imported_enum_inject_negative_bFT_OSC17NSNegativeOptions() {
// CHECK:   ret i32 -2147483648
func imported_enum_inject_negative_b() -> NSNegativeOptions {
  return .Bar
}

// CHECK: define i32 @_TF12objc_ns_enum40imported_enum_inject_negative_unsigned_aFT_OSC25NSNegativeUnsignedOptions() {
// CHECK:   ret i32 -1
func imported_enum_inject_negative_unsigned_a() -> NSNegativeUnsignedOptions {
  return .Foo
}

// CHECK: define i32 @_TF12objc_ns_enum40imported_enum_inject_negative_unsigned_bFT_OSC25NSNegativeUnsignedOptions() {
// CHECK:   ret i32 -2147483648
func imported_enum_inject_negative_unsigned_b() -> NSNegativeUnsignedOptions {
  return .Bar
}

func use_metadata<T>(t:T){}
use_metadata(NSRuncingOptions.Mince)
