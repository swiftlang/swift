// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | %FileCheck %s -DINT=i%target-ptrsize

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation
import gizmo

// CHECK: @"$SSo16NSRuncingOptionsVMn" = linkonce_odr hidden constant
// CHECK: @"$SSo16NSRuncingOptionsVN" = linkonce_odr hidden constant
//   CHECK-SAME: @"$SBi{{[0-9]+}}_WV"
// CHECK: @"$SSo16NSRuncingOptionsVSQSCMc" = linkonce_odr hidden constant %swift.protocol_conformance_descriptor { {{.*}}@"$SSo16NSRuncingOptionsVSQSCWa
// CHECK: @"$SSo28NeverActuallyMentionedByNameVSQSCWp" = linkonce_odr hidden constant

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} i32 @main
// CHECK:         call swiftcc %swift.metadata_response @"$SSo16NSRuncingOptionsVMa"(i64 0)

// CHECK: define hidden swiftcc i16 @"$S12objc_ns_enum09imported_C9_inject_aSo16NSRuncingOptionsVyF"()
// CHECK:   ret i16 123
func imported_enum_inject_a() -> NSRuncingOptions {
  return .mince
}

// CHECK: define hidden swiftcc i16 @"$S12objc_ns_enum09imported_C9_inject_bSo16NSRuncingOptionsVyF"()
// CHECK:   ret i16 4567
func imported_enum_inject_b() -> NSRuncingOptions {
  return .quinceSliced
}

// CHECK: define hidden swiftcc i16 @"$S12objc_ns_enum09imported_C9_inject_cSo16NSRuncingOptionsVyF"()
// CHECK:   ret i16 5678
func imported_enum_inject_c() -> NSRuncingOptions {
  return .quinceJulienned
}

// CHECK: define hidden swiftcc i16 @"$S12objc_ns_enum09imported_C9_inject_dSo16NSRuncingOptionsVyF"()
// CHECK:   ret i16 6789
func imported_enum_inject_d() -> NSRuncingOptions {
  return .quinceDiced
}

// CHECK: define hidden swiftcc i32 @"$S12objc_ns_enum09imported_C17_inject_radixed_aSo16NSRadixedOptionsVyF"() {{.*}} {
// -- octal 0755
// CHECK:   ret i32 493
func imported_enum_inject_radixed_a() -> NSRadixedOptions {
  return .octal
}

// CHECK: define hidden swiftcc i32 @"$S12objc_ns_enum09imported_C17_inject_radixed_bSo16NSRadixedOptionsVyF"() {{.*}} {
// -- hex 0xFFFF
// CHECK:   ret i32 65535
func imported_enum_inject_radixed_b() -> NSRadixedOptions {
  return .hex
}

// CHECK: define hidden swiftcc i32 @"$S12objc_ns_enum09imported_C18_inject_negative_aSo17NSNegativeOptionsVyF"() {{.*}} {
// CHECK:   ret i32 -1
func imported_enum_inject_negative_a() -> NSNegativeOptions {
  return .foo
}

// CHECK: define hidden swiftcc i32 @"$S12objc_ns_enum09imported_C18_inject_negative_bSo17NSNegativeOptionsVyF"() {{.*}} {
// CHECK:   ret i32 -2147483648
func imported_enum_inject_negative_b() -> NSNegativeOptions {
  return .bar
}

// CHECK: define hidden swiftcc i32 @"$S12objc_ns_enum09imported_C27_inject_negative_unsigned_aSo25NSNegativeUnsignedOptionsVyF"() {{.*}} {
// CHECK:   ret i32 -1
func imported_enum_inject_negative_unsigned_a() -> NSNegativeUnsignedOptions {
  return .foo
}

// CHECK: define hidden swiftcc i32 @"$S12objc_ns_enum09imported_C27_inject_negative_unsigned_bSo25NSNegativeUnsignedOptionsVyF"() {{.*}} {
// CHECK:   ret i32 -2147483648
func imported_enum_inject_negative_unsigned_b() -> NSNegativeUnsignedOptions {
  return .bar
}

func test_enum_without_name_Equatable(_ obj: TestThatEnumType) -> Bool {
  return obj.getValue() != .ValueOfThatEnumType
}

func use_metadata<T: Equatable>(_ t:T){}
use_metadata(NSRuncingOptions.mince)

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$SSo16NSRuncingOptionsVMa"(i64)
// CHECK:         call swiftcc %swift.metadata_response @swift_getForeignTypeMetadata([[INT]] %0, {{.*}} @"$SSo16NSRuncingOptionsVN" {{.*}}) [[NOUNWIND_READNONE:#[0-9]+]]

// CHECK-LABEL: define linkonce_odr hidden i8** @"$SSo16NSRuncingOptionsVSQSCWa"()
// CHECK:  [[NONUNIQUE:%.*]] = call i8** @swift_getGenericWitnessTable(%swift.generic_witness_table_cache* @"$SSo16NSRuncingOptionsVSQSCWG", %swift.type* null, i8*** null)
// CHECK:  [[UNIQUE:%.*]] = call i8** @swift_getForeignWitnessTable(i8** [[NONUNIQUE]], %swift.type_descriptor* bitcast (<{ {{.*}} }>* @"$SSo16NSRuncingOptionsVMn" to %swift.type_descriptor*), %swift.protocol* @"$SSQMp")
// CHECK:  ret i8** [[UNIQUE]]

@objc enum ExportedToObjC: Int {
  case Foo = -1, Bar, Bas
}

// CHECK-LABEL: define hidden swiftcc i64 @"$S12objc_ns_enum0a1_C7_injectAA14ExportedToObjCOyF"()
// CHECK:         ret i64 -1
func objc_enum_inject() -> ExportedToObjC {
  return .Foo
}

// CHECK-LABEL: define hidden swiftcc i64 @"$S12objc_ns_enum0a1_C7_switchySiAA14ExportedToObjCOF"(i64)
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
  // CHECK: define internal void @"$S12objc_ns_enum15ObjCEnumMethodsC0C2InyyAA010ExportedToD1COFTo"([[OBJC_ENUM_METHODS:.*]]*, i8*, i64)
  @objc dynamic func enumIn(_ x: ExportedToObjC) {}
  // CHECK: define internal i64 @"$S12objc_ns_enum15ObjCEnumMethodsC0C3OutAA010ExportedToD1COyFTo"([[OBJC_ENUM_METHODS]]*, i8*)
  @objc dynamic func enumOut() -> ExportedToObjC { return .Foo }

  // CHECK: define internal i64 @"$S12objc_ns_enum15ObjCEnumMethodsC4propAA010ExportedToD1COvgTo"([[OBJC_ENUM_METHODS]]*, i8*)
  // CHECK: define internal void @"$S12objc_ns_enum15ObjCEnumMethodsC4propAA010ExportedToD1COvsTo"([[OBJC_ENUM_METHODS]]*, i8*, i64)
  @objc dynamic var prop: ExportedToObjC = .Foo
}

// CHECK-LABEL: define hidden swiftcc void @"$S12objc_ns_enum0a1_C13_method_callsyyAA15ObjCEnumMethodsCF"(%T12objc_ns_enum15ObjCEnumMethodsC*)
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

