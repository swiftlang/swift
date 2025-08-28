// RUN: %target-swift-emit-sil %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo | %FileCheck %s --check-prefix CHECK-SIL
// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo | %FileCheck %s --check-prefix CHECK-IR

// UNSUPPORTED: CPU=wasm32
// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

precedencegroup AssignmentPrecedence { assignment: true }

public struct T1 {}
public struct T2 { var x: Builtin.Int1 }

public class MyClass<T> {
  var t: T
  init(t: T) { self.t = t }
}

public func foo(t: T1) -> MyClass<T1> {
  return MyClass<T1>(t: t)
}

public func bar(t: T2) -> MyClass<T2> {
  return MyClass<T2>(t: t)
}

// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassC1txvgAA2T1V_Tg5 {{.*}}{
// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassC1txvsAA2T1V_Tg5 {{.*}}{
// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassC1txvMAA2T1V_Tg5 {{.*}}{
// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassC1tACyxGx_tcfCAA2T1V_Tt0g5 {{.*}}{
// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassCfDAA2T1V_Tg5 {{.*}}{

// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassC1txvgAA2T2V_Tg5 {{.*}}{
// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassC1txvsAA2T2V_Tg5 {{.*}}{
// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassC1txvMAA2T2V_Tg5 {{.*}}{
// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassC1tACyxGx_tcfCAA2T2V_Tt0g5 {{.*}}{
// CHECK-SIL-DAG: sil {{.*}}@$e4main7MyClassCfDAA2T2V_Tg5 {{.*}}{

// CHECK-SIL: sil_vtable $MyClass<T2> {
// CHECK-SIL:   #MyClass.t!getter: <T> (MyClass<T>) -> () -> T : @$e4main7MyClassC1txvgAA2T2V_Tg5 // specialized MyClass.t.getter
// CHECK-SIL:   #MyClass.t!setter: <T> (MyClass<T>) -> (T) -> () : @$e4main7MyClassC1txvsAA2T2V_Tg5 // specialized MyClass.t.setter
// CHECK-SIL:   #MyClass.t!modify: <T> (MyClass<T>) -> () -> () : @$e4main7MyClassC1txvMAA2T2V_Tg5  // specialized MyClass.t.modify
// CHECK-SIL:   #MyClass.init!allocator: <T> (MyClass<T>.Type) -> (T) -> MyClass<T> : @$e4main7MyClassC1tACyxGx_tcfCAA2T2V_Tg5  // specialized MyClass.__allocating_init(t:)
// CHECK-SIL:   #MyClass.deinit!deallocator: @$e4main7MyClassCfDAA2T2V_Tg5  // specialized MyClass.__deallocating_deinit
// CHECK-SIL: }

// CHECK-SIL: sil_vtable $MyClass<T1> {
// CHECK-SIL:   #MyClass.t!getter: <T> (MyClass<T>) -> () -> T : @$e4main7MyClassC1txvgAA2T1V_Tg5 // specialized MyClass.t.getter
// CHECK-SIL:   #MyClass.t!setter: <T> (MyClass<T>) -> (T) -> () : @$e4main7MyClassC1txvsAA2T1V_Tg5 // specialized MyClass.t.setter
// CHECK-SIL:   #MyClass.t!modify: <T> (MyClass<T>) -> () -> () : @$e4main7MyClassC1txvMAA2T1V_Tg5  // specialized MyClass.t.modify
// CHECK-SIL:   #MyClass.init!allocator: <T> (MyClass<T>.Type) -> (T) -> MyClass<T> : @$e4main7MyClassC1tACyxGx_tcfCAA2T1V_Tg5  // specialized MyClass.__allocating_init(t:)
// CHECK-SIL:   #MyClass.deinit!deallocator: @$e4main7MyClassCfDAA2T1V_Tg5  // specialized MyClass.__deallocating_deinit
// CHECK-SIL: }


// CHECK-IR-DAG: @"$e4main7MyClassCyAA2T2VGN" = {{.*}}<{ ptr, ptr, ptr, ptr, ptr, ptr, ptr }> <{ ptr null, ptr @"$e4main7MyClassCfDAA2T2V_Tg5", ptr null, ptr @"$e4main7MyClassC1txvgAA2T2V_Tg5", ptr @"$e4main7MyClassC1txvsAA2T2V_Tg5", ptr @"$e4main7MyClassC1txvMAA2T2V_Tg5", ptr @"$e4main7MyClassC1tACyxGx_tcfCAA2T2V_Tg5" }>
// CHECK-IR-DAG: @"$e4main7MyClassCyAA2T1VGN" = {{.*}}<{ ptr, ptr, ptr, ptr, ptr, ptr, ptr }> <{ ptr null, ptr @"$e4main7MyClassCfDAA2T1V_Tg5", ptr null, ptr @"$e4main7MyClassC1txvgAA2T1V_Tg5", ptr @"$e4main7MyClassC1txvsAA2T1V_Tg5", ptr @"$e4main7MyClassC1txvMAA2T1V_Tg5", ptr @"$e4main7MyClassC1tACyxGx_tcfCAA2T1V_Tg5" }>

// CHECK-IR-DAG: define {{.*}}void @"$e4main7MyClassC1txvgAA2T1V_Tg5"(ptr swiftself %0)
// CHECK-IR-DAG: define {{.*}}i1 @"$e4main7MyClassC1txvgAA2T2V_Tg5"(ptr swiftself %0)
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main7MyClassC1tACyxGx_tcfCAA2T1V_Tg5"(ptr swiftself %0)
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main7MyClassC1tACyxGx_tcfCAA2T2V_Tg5"(i1 %0, ptr swiftself %1)
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main7MyClassC1tACyxGx_tcfCAA2T1V_Tt0g5"()
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main7MyClassC1tACyxGx_tcfCAA2T2V_Tt0g5"(i1 %0)
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main7MyClassC1tACyxGx_tcfcAA2T1V_Tg5"(ptr swiftself %0)
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main7MyClassC1tACyxGx_tcfcAA2T2V_Tg5"(i1 %0, ptr swiftself %1)
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main7MyClassCfdAA2T1V_Tg5"(ptr swiftself %0)
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main7MyClassCfdAA2T2V_Tg5"(ptr swiftself %0)
// CHECK-IR-DAG: define {{.*}}void @"$e4main7MyClassCfDAA2T1V_Tg5"(ptr swiftself %0)
// CHECK-IR-DAG: define {{.*}}void @"$e4main7MyClassCfDAA2T2V_Tg5"(ptr swiftself %0)
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main3foo1tAA7MyClassCyAA2T1VGAG_tF"()
// CHECK-IR-DAG: define {{.*}}ptr @"$e4main3bar1tAA7MyClassCyAA2T2VGAG_tF"(i1 %0)
