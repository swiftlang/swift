// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -D COM_INTEROP -sil-verify-all -emit-silgen %s | %FileCheck %s --check-prefixes=CHECK,NATIVE
// RUN: %target-swift-frontend -sil-verify-all -emit-silgen %s | %FileCheck %s --check-prefix=NATIVE

#if COM_INTEROP
@com(interface: "43000000-0000-0000-0000-000000000001")
protocol IWidget {
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11dynamicType2ofypXpAA7IWidget_p_tF
// CHECK-SAME:  @guaranteed any IWidget
// CHECK:       [[TYPE:%.*]] = existential_metatype $@thick any Any.Type, %{{.*}}
// CHECK:       return [[TYPE]]
func dynamicType(of value: borrowing any IWidget) -> Any.Type {
  type(of: value)
}
#endif

protocol Native {}
class NativeClass {}
struct NativeValue {}

// Native result types are unchanged with COM interop enabled or disabled.
// NATIVE-LABEL: sil hidden [ossa] {{.*}}nativeExistential
// NATIVE: [[TYPE:%.*]] = existential_metatype $@thick any Native.Type, %{{.*}}
// NATIVE: return [[TYPE]]
func nativeExistential(_ value: borrowing any Native) -> any Native.Type {
  type(of: value)
}

// NATIVE-LABEL: sil hidden [ossa] {{.*}}nativeClass
// NATIVE: [[TYPE:%.*]] = value_metatype $@thick NativeClass.Type, %{{.*}}
// NATIVE: return [[TYPE]]
func nativeClass(_ value: borrowing NativeClass) -> NativeClass.Type {
  type(of: value)
}

// NATIVE-LABEL: sil hidden [ossa] {{.*}}nativeGeneric
// NATIVE: [[TYPE:%.*]] = value_metatype $@thick T.Type, %{{.*}}
// NATIVE: return [[TYPE]]
func nativeGeneric<T>(_ value: borrowing T) -> T.Type {
  type(of: value)
}

// NATIVE-LABEL: sil hidden [ossa] {{.*}}nativeValue
// NATIVE: [[TYPE:%.*]] = metatype $@thin NativeValue.Type
// NATIVE: return [[TYPE]]
func nativeValue(_ value: NativeValue) -> NativeValue.Type {
  type(of: value)
}
