// RUN: %target-swift-emit-ir %s -parse-stdlib -module-name Swift -enable-experimental-feature Embedded -wmo -target arm64e-apple-none | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

class MyClass {}

struct MyStruct {
  var c: MyClass
}

public func foo() -> Builtin.Int1 {
  return Builtin.ispod(MyStruct.self)
}

// CHECK:      define {{.*}}i1 @"$es3fooBi1_yF"()
// CHECK-NEXT: entry:
// CHECK-NEXT:   ret i1 false
// CHECK-NEXT: }

public func bar() -> Builtin.Int1 {
  var s = MyGenericStruct<MyStruct>()
  return s.foo()
}

public struct MyGenericStruct<T> {
  public func foo() -> Builtin.Int1 {
    return Builtin.ispod(T.self)
  }
}

// CHECK:      define {{.*}}i1 @"$es15MyGenericStructV3fooBi1_yFs0aC0V_Tg5"()
// CHECK-NEXT: entry:
// CHECK-NEXT:   ret i1 false
// CHECK-NEXT: }
