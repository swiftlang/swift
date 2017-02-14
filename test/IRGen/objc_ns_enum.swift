// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -new-mangling-for-tests -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation
import gizmo

// CHECK: @_T0SC16NSRuncingOptionsOWV = linkonce_odr hidden constant
// CHECK: @_T0SC16NSRuncingOptionsOMn = linkonce_odr hidden constant
// CHECK: @_T0SC16NSRuncingOptionsON = linkonce_odr hidden global
// CHECK: @_T0SC28NeverActuallyMentionedByNameOs9Equatable5gizmoWP = linkonce_odr hidden constant

// CHECK-LABEL: define{{( protected)?}} i32 @main
// CHECK:         call %swift.type* @_T0SC16NSRuncingOptionsOMa()

// CHECK: define hidden swiftcc i16 @_T012objc_ns_enum09imported_C9_inject_aSC16NSRuncingOptionsOyF()
// CHECK:   ret i16 123
func imported_enum_inject_a() -> NSRuncingOptions {
  return .mince
}

// CHECK: define hidden swiftcc i16 @_T012objc_ns_enum09imported_C9_inject_bSC16NSRuncingOptionsOyF()
// CHECK:   ret i16 4567
func imported_enum_inject_b() -> NSRuncingOptions {
  return .quinceSliced
}

// CHECK: define hidden swiftcc i16 @_T012objc_ns_enum09imported_C9_inject_cSC16NSRuncingOptionsOyF()
// CHECK:   ret i16 5678
func imported_enum_inject_c() -> NSRuncingOptions {
  return .quinceJulienned
}

// CHECK: define hidden swiftcc i16 @_T012objc_ns_enum09imported_C9_inject_dSC16NSRuncingOptionsOyF()
// CHECK:   ret i16 6789
func imported_enum_inject_d() -> NSRuncingOptions {
  return .quinceDiced
}

// CHECK: define hidden swiftcc i32 @_T012objc_ns_enum09imported_C17_inject_radixed_aSC16NSRadixedOptionsOyF() {{.*}} {
// -- octal 0755
// CHECK:   ret i32 493
func imported_enum_inject_radixed_a() -> NSRadixedOptions {
  return .octal
}

// CHECK: define hidden swiftcc i32 @_T012objc_ns_enum09imported_C17_inject_radixed_bSC16NSRadixedOptionsOyF() {{.*}} {
// -- hex 0xFFFF
// CHECK:   ret i32 65535
func imported_enum_inject_radixed_b() -> NSRadixedOptions {
  return .hex
}

// CHECK: define hidden swiftcc i32 @_T012objc_ns_enum09imported_C18_inject_negative_aSC17NSNegativeOptionsOyF() {{.*}} {
// CHECK:   ret i32 -1
func imported_enum_inject_negative_a() -> NSNegativeOptions {
  return .foo
}

// CHECK: define hidden swiftcc i32 @_T012objc_ns_enum09imported_C18_inject_negative_bSC17NSNegativeOptionsOyF() {{.*}} {
// CHECK:   ret i32 -2147483648
func imported_enum_inject_negative_b() -> NSNegativeOptions {
  return .bar
}

// CHECK: define hidden swiftcc i32 @_T012objc_ns_enum09imported_C27_inject_negative_unsigned_aSC25NSNegativeUnsignedOptionsOyF() {{.*}} {
// CHECK:   ret i32 -1
func imported_enum_inject_negative_unsigned_a() -> NSNegativeUnsignedOptions {
  return .foo
}

// CHECK: define hidden swiftcc i32 @_T012objc_ns_enum09imported_C27_inject_negative_unsigned_bSC25NSNegativeUnsignedOptionsOyF() {{.*}} {
// CHECK:   ret i32 -2147483648
func imported_enum_inject_negative_unsigned_b() -> NSNegativeUnsignedOptions {
  return .bar
}

func test_enum_without_name_Equatable(_ obj: TestThatEnumType) -> Bool {
  return obj.getValue() != .ValueOfThatEnumType
}

func use_metadata<T>(_ t:T){}
use_metadata(NSRuncingOptions.mince)

// CHECK-LABEL: define linkonce_odr hidden %swift.type* @_T0SC16NSRuncingOptionsOMa()
// CHECK:         call %swift.type* @swift_getForeignTypeMetadata({{.*}} @_T0SC16NSRuncingOptionsON {{.*}}) [[NOUNWIND_READNONE:#[0-9]+]]

@objc enum ExportedToObjC: Int {
  case Foo = -1, Bar, Bas
}

// CHECK-LABEL: define hidden swiftcc i64 @_T012objc_ns_enum0a1_C7_injectAA14ExportedToObjCOyF()
// CHECK:         ret i64 -1
func objc_enum_inject() -> ExportedToObjC {
  return .Foo
}

// CHECK-LABEL: define hidden swiftcc i64 @_T012objc_ns_enum0a1_C7_switchSiAA14ExportedToObjCOF(i64)
// CHECK:         switch i64 %0, label {{%.*}} [
// CHECK:           i64 -1, label {{%.*}}
// CHECK:           i64  0, label {{%.*}}
// CHECK:           i64  1, label {{%.*}}
func objc_enum_switch(_ x: ExportedToObjC) -> Int {
  switch x {
  case .Foo:
    return 0
  case .Bar:
    return 1
  case .Bas:
    return 2
  }
}

@objc class ObjCEnumMethods : NSObject {
  // CHECK: define internal void @_T012objc_ns_enum15ObjCEnumMethodsC0C2InyAA010ExportedToD1COFTo([[OBJC_ENUM_METHODS:.*]]*, i8*, i64)
  dynamic func enumIn(_ x: ExportedToObjC) {}
  // CHECK: define internal i64 @_T012objc_ns_enum15ObjCEnumMethodsC0C3OutAA010ExportedToD1COyFTo([[OBJC_ENUM_METHODS]]*, i8*)
  dynamic func enumOut() -> ExportedToObjC { return .Foo }

  // CHECK: define internal i64 @_T012objc_ns_enum15ObjCEnumMethodsC4propAA010ExportedToD1COfgTo([[OBJC_ENUM_METHODS]]*, i8*)
  // CHECK: define internal void @_T012objc_ns_enum15ObjCEnumMethodsC4propAA010ExportedToD1COfsTo([[OBJC_ENUM_METHODS]]*, i8*, i64)
  dynamic var prop: ExportedToObjC = .Foo
}

// CHECK-LABEL: define hidden swiftcc void @_T012objc_ns_enum0a1_C13_method_callsyAA15ObjCEnumMethodsCF(%C12objc_ns_enum15ObjCEnumMethods*)
func objc_enum_method_calls(_ x: ObjCEnumMethods) {
  
  // CHECK: call i64 bitcast (void ()* @objc_msgSend to i64 ([[OBJC_ENUM_METHODS]]*, i8*)*)
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[OBJC_ENUM_METHODS]]*, i8*, i64)*)
  x.enumIn(x.enumOut())
  // CHECK: call i64 bitcast (void ()* @objc_msgSend to i64 ([[OBJC_ENUM_METHODS]]*, i8*)*)
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[OBJC_ENUM_METHODS]]*, i8*, i64)*)
  x.enumIn(x.prop)
  // CHECK: call i64 bitcast (void ()* @objc_msgSend to i64 ([[OBJC_ENUM_METHODS]]*, i8*)*)
  // CHECK: call void bitcast (void ()* @objc_msgSend to void ([[OBJC_ENUM_METHODS]]*, i8*, i64)*)
  x.prop = x.enumOut()
}

// CHECK: attributes [[NOUNWIND_READNONE]] = { nounwind readnone }

