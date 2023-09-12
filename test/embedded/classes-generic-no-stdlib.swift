// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none | %FileCheck %s

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

// CHECK: @"$s4main7MyClassCyAA2T2VGN" = {{.*}}<{ ptr, ptr, ptr, ptr, ptr, ptr }> <{ ptr null, ptr @"$s4main7MyClassCfDAA2T1V_Tg5", ptr @"$s4main7MyClassC1txvgAA2T1V_Tg5", ptr @"$s4main7MyClassC1txvsAA2T1V_Tg5", ptr @"$s4main7MyClassC1txvMAA2T1V_Tg5", ptr @"$s4main7MyClassC1tACyxGx_tcfCAA2T1V_Tg5" }>
// CHECK: @"$s4main7MyClassCyAA2T1VGN" = {{.*}}<{ ptr, ptr, ptr, ptr, ptr, ptr }> <{ ptr null, ptr @"$s4main7MyClassCfDAA2T1V_Tg5", ptr @"$s4main7MyClassC1txvgAA2T1V_Tg5", ptr @"$s4main7MyClassC1txvsAA2T1V_Tg5", ptr @"$s4main7MyClassC1txvMAA2T1V_Tg5", ptr @"$s4main7MyClassC1tACyxGx_tcfCAA2T1V_Tg5" }>

// CHECK: define {{.*}}void @"$s4main7MyClassC1txvgAA2T1V_Tg5"(ptr swiftself %0)
// CHECK: define {{.*}}void @"$s4main7MyClassC1txvsAA2T1V_Tg5"(ptr swiftself %0)
// CHECK: define {{.*}}ptr @"$s4main2T1VWOd"(ptr %0, ptr %1)
// CHECK: define {{.*}}ptr @"$s4main7MyClassC1tACyxGx_tcfCAA2T1V_Tgm5"()
// CHECK: define {{.*}}ptr @"$s4main7MyClassC1tACyxGx_tcfCAA2T2V_Tgm5"(i1 %0)
// CHECK: define {{.*}}ptr @"$s4main7MyClassC1tACyxGx_tcfCAA2T1V_Tg5"(ptr swiftself %0)
// CHECK: define {{.*}}ptr @"$s4main7MyClassC1tACyxGx_tcfcAA2T1V_Tg5"(ptr swiftself %0)
// CHECK: define {{.*}}ptr @"$s4main7MyClassC1tACyxGx_tcfcAA2T2V_Tg5"(i1 %0, ptr swiftself %1)
// CHECK: define {{.*}}void @"$s4main7MyClassCfDAA2T1V_Tg5"(ptr swiftself %0)
// CHECK: define {{.*}}ptr @"$s4main3foo1tAA7MyClassCyAA2T1VGAG_tF"()
// CHECK: define {{.*}}ptr @"$s4main3bar1tAA7MyClassCyAA2T2VGAG_tF"(i1 %0)
