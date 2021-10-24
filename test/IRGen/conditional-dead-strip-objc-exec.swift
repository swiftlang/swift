// Tests that with -conditional-runtime-records, LLVM GlobalDCE is able to
// remove unused classes, protocols and protocol conformances.

// RUN: %empty-directory(%t)

// RUN: %target-build-swift \
// RUN:     -Xfrontend -conditional-runtime-records \
// RUN:     %s -emit-ir -o %t/main.ll

// RUN: %target-clang %t/main.ll -isysroot %sdk -L%swift_obj_root/lib/swift/%target-sdk-name -flto -o %t/main
// RUN: %target-run %t/main | %FileCheck %s

// RUN: %llvm-nm --defined-only %t/main | %FileCheck %s --check-prefix=NM

// REQUIRES: executable_test

// Test disabled until LLVM GlobalDCE supports conditional references.
// REQUIRES: rdar81868900

class MyUnusedClass {
	func unused_method() {}
}

print("Hello!")

// CHECK: Hello!

// NM-NOT: $s4main13MyUnusedClassCMf
// NM-NOT: $s4main13MyUnusedClassCMm
