// Tests that under -enable-llvm-wme, LLVM GlobalDCE is able to remove unused
// witness methods, and that used witness methods are not removed (by running
// the program).

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-objc-interop -Xfrontend -enable-llvm-wme %s -emit-ir -o %t/main.ll
// RUN: %target-clang %t/main.ll -isysroot %sdk -L%swift_obj_root/lib/swift/%target-sdk-name -flto -o %t/main
// RUN: %target-run %t/main | %FileCheck %s

// RUN: %llvm-nm --defined-only %t/main | %FileCheck %s --check-prefix=NM

// REQUIRES: executable_test

// Test disabled until LLVM GlobalDCE supports Swift vtables.
// REQUIRES: rdar81868930

protocol TheProtocol {
  func func1_live()
  func func2_dead()
}

struct MyStruct : TheProtocol {
  func func1_live() { print("MyStruct.func1_live") }
  func func2_dead() { print("MyStruct.func2_dead") }
}

let x: TheProtocol = MyStruct()
x.func1_live()
// CHECK: MyStruct.func1_live

// NM:     $s4main8MyStructV10func1_liveyyF
// NM-NOT: $s4main8MyStructV10func2_deadyyF
