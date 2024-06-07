// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo | %FileCheck %s

// REQUIRES: swift_in_compiler

public class MyClass {
  func foo() { }
  func bar() { }
}

public class MySubClass: MyClass {
  override func foo() { }
}

// CHECK: @"$s4main7MyClassCN" = {{.*}}<{ ptr, ptr, ptr, ptr, ptr, ptr }> <{ ptr null, ptr @"$s4main7MyClassCfD", ptr null, ptr @"$s4main7MyClassC3fooyyF", ptr @"$s4main7MyClassC3baryyF", ptr @swift_deletedMethodError }>
// CHECK: @"$s4main10MySubClassCN" = {{.*}}<{ ptr, ptr, ptr, ptr, ptr, ptr }> <{ ptr @"$s4main7MyClassCN", ptr @"$s4main10MySubClassCfD", ptr null, ptr @"$s4main10MySubClassC3fooyyF", ptr @"$s4main7MyClassC3baryyF", ptr @"$s4main10MySubClassCACycfC" }>

// CHECK: define {{.*}}void @"$s4main4test1xyAA7MyClassC_tF"(ptr %0)
public func test(x: MyClass) {

  x.foo() // goes through the vtable
  // CHECK: %1 = load ptr, ptr %0
  // CHECK: %2 = getelementptr inbounds ptr, ptr %1, i64 3
  // CHECK: %3 = load ptr, ptr %2
  // CHECK: call swiftcc void %3(ptr swiftself %0)

  x.bar() // does not go through the vtable
  // CHECK: call swiftcc void @"$s4main7MyClassC3baryyF"

  let y = MySubClass()
  // CHECK: call swiftcc ptr @"$s4main10MySubClassCACycfC"

  y.foo() // does not go through the vtable
  // CHECK: call swiftcc void @"$s4main10MySubClassC3fooyyF"

  y.bar() // does not go through the vtable
  // CHECK: call swiftcc void @"$s4main7MyClassC3baryyF"

}
