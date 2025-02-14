// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -Xllvm -sil-disable-pass=function-signature-opts %s -parse-as-library -enable-experimental-feature Embedded -module-name main -emit-irgen | %FileCheck %s --check-prefix CHECK-IR
// RUN: %target-swift-frontend -O -Xllvm -sil-disable-pass=function-signature-opts %s -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public class MyClass {
  public init() { print("MyClass.init") }
  deinit { print("MyClass.deinit") }
  public func foo() { print("MyClass.foo") }
}

public class MySubClass: MyClass {
  public override init() { print("MySubClass.init") }
  deinit { print("MySubClass.deinit") }
  public override func foo() { print("MySubClass.foo") }
}

@inline(never)
public func bar(o: MyClass) {
  o.foo()
}

final public class MyFinalClass {
  public init() { print("MyFinalClass.init") }
  deinit { print("MyFinalClass.deinit") }
  public func foo() { print("MyFinalClass.foo") }
}

public struct MyStruct {
  @inline(never)
  init(_ c: MyFinalClass) {
    c.foo()
  }
}

@main
struct Main {
  static func test1() {
    let o = MySubClass()
    // CHECK: MySubClass.init
    // CHECK: MyClass.init
    o.foo()
    // CHECK: MySubClass.foo
    bar(o: o)
    // CHECK: MySubClass.foo
    // CHECK: MySubClass.deinit
    // CHECK: MyClass.deinit
  }

  static func test2() -> MyStruct {
    let c = MyFinalClass()
    // CHECK: MyFinalClass.init
    return MyStruct(c)
    // CHECK: MyFinalClass.foo
    // CHECK: MyFinalClass.deinit
  }

  static func main() {
    test1()
    print("")
    test2()
  }
}

// Check that stack promotion did really happen

// CHECK-IR:      define {{.*}}@"$e4main8MyStructVyAcA0B10FinalClassCcfC"
// CHECK-IR-NEXT: entry:
// CHECK-IR-NEXT:   call {{.*}}@"$es5print_10terminatorys12StaticStringV_ADtF"
// CHECK-IR-NEXT:   call {{.*}}@swift_release
// CHECK-IR-NEXT:   ret void
// CHECK-IR-NEXT: }

// CHECK-IR:      define {{.*}}@main
// CHECK-IR-NEXT: entry:
// CHECK-IR-NEXT:   alloca %T4main10MySubClassC
// CHECK-IR-NEXT:   alloca %T4main12MyFinalClassC
// CHECK-IR-NEXT:   call {{.*}}@swift_initStackObject
// CHECK-IR-NEXT:   call {{.*}}@"$es5print_10terminatorys12StaticStringV_ADtF"
// CHECK-IR-NEXT:   call {{.*}}@"$es5print_10terminatorys12StaticStringV_ADtF"
// CHECK-IR-NEXT:   call {{.*}}@"$es5print_10terminatorys12StaticStringV_ADtF"
// CHECK-IR-NEXT:   call {{.*}}@"$e4main3bar1oyAA7MyClassC_tF"
// CHECK-IR-NEXT:   call {{.*}}@swift_setDeallocating
// CHECK-IR-NEXT:   call {{.*}}@"$es5print_10terminatorys12StaticStringV_ADtF"
// CHECK-IR-NEXT:   call {{.*}}@"$es5print_10terminatorys12StaticStringV_ADtF"
// CHECK-IR-NEXT:   call {{.*}}@llvm.lifetime.end.p0
// CHECK-IR-NEXT:   call {{.*}}@"$es5print_10terminatorys12StaticStringV_ADtF"
// CHECK-IR-NEXT:   call {{.*}}@swift_initStackObject
// CHECK-IR-NEXT:   call {{.*}}@"$es5print_10terminatorys12StaticStringV_ADtF"
// CHECK-IR-NEXT:   call {{.*}}@"$e4main8MyStructVyAcA0B10FinalClassCcfC"
// CHECK-IR-NEXT:   call {{.*}}@llvm.lifetime.end.p0
// CHECK-IR-NEXT:   ret {{.*}}0
// CHECK-IR-NEXT: }
