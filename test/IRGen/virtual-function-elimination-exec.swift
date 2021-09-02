// Tests that under -enable-llvm-vfe, LLVM GlobalDCE is able to remove unused
// virtual methods, and that used virtual methods are not removed (by running
// the program).

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-objc-interop -Xfrontend -enable-llvm-vfe %s -emit-ir -o %t/main.ll
// RUN: %target-clang %t/main.ll -isysroot %sdk -L%swift_obj_root/lib/swift/%target-sdk-name -flto -o %t/main
// RUN: %target-run %t/main | %FileCheck %s

// RUN: %llvm-nm --defined-only %t/main | %FileCheck %s --check-prefix=NM

// REQUIRES: executable_test

// Test disabled until LLVM GlobalDCE supports Swift vtables.
// REQUIRES: rdar81868930

class MyClass {
  func foo() { print("MyClass.foo") }
  func bar() { print("MyClass.bar") }
}

class MyDerivedClass: MyClass {
  override func foo() { print("MyDerivedClass.foo") }
  override func bar() { print("MyDerivedClass.bar") }
}

let o: MyClass = MyDerivedClass()
o.foo()
// CHECK: MyDerivedClass.foo

// NM-NOT: $s4main14MyDerivedClassC3baryyF
// NM:     $s4main14MyDerivedClassC3fooyyF
// NM-NOT: $s4main7MyClassC3baryyF
// NM:     $s4main7MyClassC3fooyyF
