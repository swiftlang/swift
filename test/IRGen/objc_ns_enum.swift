// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %swift -target x86_64-apple-macosx10.9 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I %t -primary-file %s -emit-ir | FileCheck %s

import Foundation
import gizmo

// CHECK: @_TWVOSC16NSRuncingOptions = linkonce_odr hidden constant
// CHECK: @_TMnOSC16NSRuncingOptions = linkonce_odr hidden constant
// CHECK: @_TMdOSC16NSRuncingOptions = linkonce_odr hidden global
// CHECK: @_TWPOSC28NeverActuallyMentionedByNameSs9Equatable5gizmo = linkonce_odr hidden constant

// CHECK-LABEL: define i32 @main
// CHECK:         call %swift.type* @swift_getForeignTypeMetadata({{.*}} @_TMdOSC16NSRuncingOptions {{.*}}) [[NOUNWIND_READNONE:#[0-9]+]]

// CHECK: define hidden i16 @_TF12objc_ns_enum22imported_enum_inject_aFT_OSC16NSRuncingOptions()
// CHECK:   ret i16 123
func imported_enum_inject_a() -> NSRuncingOptions {
  return .Mince
}

// CHECK: define hidden i16 @_TF12objc_ns_enum22imported_enum_inject_bFT_OSC16NSRuncingOptions()
// CHECK:   ret i16 4567
func imported_enum_inject_b() -> NSRuncingOptions {
  return .QuinceSliced
}

// CHECK: define hidden i16 @_TF12objc_ns_enum22imported_enum_inject_cFT_OSC16NSRuncingOptions()
// CHECK:   ret i16 5678
func imported_enum_inject_c() -> NSRuncingOptions {
  return .QuinceJulienned
}

// CHECK: define hidden i16 @_TF12objc_ns_enum22imported_enum_inject_dFT_OSC16NSRuncingOptions()
// CHECK:   ret i16 6789
func imported_enum_inject_d() -> NSRuncingOptions {
  return .QuinceDiced
}

// CHECK: define hidden i32 @_TF12objc_ns_enum30imported_enum_inject_radixed_aFT_OSC16NSRadixedOptions() {
// -- octal 0755
// CHECK:   ret i32 493
func imported_enum_inject_radixed_a() -> NSRadixedOptions {
  return .Octal
}

// CHECK: define hidden i32 @_TF12objc_ns_enum30imported_enum_inject_radixed_bFT_OSC16NSRadixedOptions() {
// -- hex 0xFFFF
// CHECK:   ret i32 65535
func imported_enum_inject_radixed_b() -> NSRadixedOptions {
  return .Hex
}

// CHECK: define hidden i32 @_TF12objc_ns_enum31imported_enum_inject_negative_aFT_OSC17NSNegativeOptions() {
// CHECK:   ret i32 -1
func imported_enum_inject_negative_a() -> NSNegativeOptions {
  return .Foo
}

// CHECK: define hidden i32 @_TF12objc_ns_enum31imported_enum_inject_negative_bFT_OSC17NSNegativeOptions() {
// CHECK:   ret i32 -2147483648
func imported_enum_inject_negative_b() -> NSNegativeOptions {
  return .Bar
}

// CHECK: define hidden i32 @_TF12objc_ns_enum40imported_enum_inject_negative_unsigned_aFT_OSC25NSNegativeUnsignedOptions() {
// CHECK:   ret i32 -1
func imported_enum_inject_negative_unsigned_a() -> NSNegativeUnsignedOptions {
  return .Foo
}

// CHECK: define hidden i32 @_TF12objc_ns_enum40imported_enum_inject_negative_unsigned_bFT_OSC25NSNegativeUnsignedOptions() {
// CHECK:   ret i32 -2147483648
func imported_enum_inject_negative_unsigned_b() -> NSNegativeUnsignedOptions {
  return .Bar
}

func test_enum_without_name_Equatable(obj: TestThatEnumType) -> Bool {
  return obj.getValue() != .ValueOfThatEnumType
}

func use_metadata<T>(t:T){}
use_metadata(NSRuncingOptions.Mince)

@objc enum ExportedToObjC: Int {
  case Foo = -1, Bar, Bas
}

// CHECK-LABEL: define hidden i64 @_TF12objc_ns_enum16objc_enum_injectFT_OS_14ExportedToObjC()
// CHECK:         ret i64 -1
func objc_enum_inject() -> ExportedToObjC {
  return .Foo
}

// CHECK-LABEL: define hidden i64 @_TF12objc_ns_enum16objc_enum_switchFOS_14ExportedToObjCSi(i64)
// CHECK:         switch i64 %0, label {{%.*}} [
// CHECK:           i64 -1, label {{%.*}}
// CHECK:           i64  0, label {{%.*}}
// CHECK:           i64  1, label {{%.*}}
func objc_enum_switch(x: ExportedToObjC) -> Int {
  switch x {
  case .Foo:
    return 0
  case .Bar:
    return 1
  case .Bas:
    return 2
  }
}

// CHECK: attributes [[NOUNWIND_READNONE]] = { nounwind readnone }


