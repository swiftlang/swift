// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/objc_direct.m -I %S/../Inputs -c -o %t/objc_direct.o
// RUN: %target-build-swift -import-objc-header %S/../Inputs/objc_direct.h -Xlinker %t/objc_direct.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

protocol BarProtocol {
    func directProtocolMethod() -> String!
}

extension Bar: BarProtocol {}

let bar = Bar()

bar.directProperty = 123
print(bar.directProperty)
// CHECK: 123

bar.directProperty2 = 456
print(bar.directProperty2)
// CHECK: 456

bar[0] = 0
print(bar[0])
// CHECK: 789

print(bar.directMethod())
// CHECK: called directMethod

print(bar.directMethod2())
// CHECK: called directMethod2

print(Bar.directClassMethod())
// CHECK: called directClassMethod

print(Bar.directClassMethod2())
// CHECK: called directClassMethod2

print(bar.directProtocolMethod())
// CHECK: called directProtocolMethod
