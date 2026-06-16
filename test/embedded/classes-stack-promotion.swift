// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -Xllvm -sil-disable-pass=function-signature-opts %s -parse-as-library -enable-experimental-feature Embedded -module-name main -emit-irgen | %FileCheck %s --check-prefix CHECK-IR
// RUN: %target-swift-frontend -O -Xllvm -sil-disable-pass=function-signature-opts %s -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// Use an @inline(never) wrapper so each event produces a single stable call
// instruction in the IR regardless of how print() is lowered on a given platform.
@inline(never)
func output(_ s: StaticString) { print(s) }

public class MyClass {
  public init() { output("MyClass.init") }
  deinit { output("MyClass.deinit") }
  public func foo() { output("MyClass.foo") }
}

public class MySubClass: MyClass {
  public override init() { output("MySubClass.init") }
  deinit { output("MySubClass.deinit") }
  public override func foo() { output("MySubClass.foo") }
}

@inline(never)
public func bar(o: MyClass) {
  o.foo()
}

final public class MyFinalClass {
  public init() { output("MyFinalClass.init") }
  deinit { output("MyFinalClass.deinit") }
  public func foo() { output("MyFinalClass.foo") }
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
    test2()
  }
}

// Check that stack promotion did really happen

// CHECK-IR:      define {{.*}}@"$e4main8MyStructVyAcA0B10FinalClassCcfC"
// CHECK-IR-NEXT: entry:
// CHECK-IR-NEXT:   call {{.*}}@"$e4main6output
// CHECK-IR-NEXT:   call {{.*}}@swift_release
// CHECK-IR-NEXT:   ret void
// CHECK-IR-NEXT: }

// CHECK-IR:      define {{.*}}i32 @{{(main|__main_argc_argv)}}(
// CHECK-IR-NEXT: entry:
// CHECK-IR-NEXT:   alloca %T4main10MySubClassC
// CHECK-IR-NEXT:   alloca %T4main12MyFinalClassC
// CHECK-IR-NEXT:   call {{.*}}@swift_initStackObject
// CHECK-IR-NEXT:   call {{.*}}@"$e4main6output
// CHECK-IR-NEXT:   call {{.*}}@"$e4main6output
// CHECK-IR-NEXT:   call {{.*}}@"$e4main6output
// CHECK-IR-NEXT:   call {{.*}}@"$e4main3bar1oyAA7MyClassC_tF"
// CHECK-IR-NEXT:   call {{.*}}@swift_setDeallocating
// CHECK-IR-NEXT:   call {{.*}}@"$e4main6output
// CHECK-IR-NEXT:   call {{.*}}@"$e4main6output
// CHECK-IR-NEXT:   call {{.*}}@llvm.lifetime.end.p0
// CHECK-IR-NEXT:   call {{.*}}@swift_initStackObject
// CHECK-IR-NEXT:   call {{.*}}@"$e4main6output
// CHECK-IR-NEXT:   call {{.*}}@"$e4main8MyStructVyAcA0B10FinalClassCcfC"
// CHECK-IR-NEXT:   call {{.*}}@llvm.lifetime.end.p0
// CHECK-IR-NEXT:   ret
// CHECK-IR-NEXT: }
