// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/reference_storage -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/reference_storage -type-from-mangled=%t/input | %FileCheck %s

func blackHole(_: Any...) {}

class Class {}


let c = Class()

weak var weakVar: Class? = c
unowned let unownedVar: Class = c
unowned(unsafe) let unmanagedVar: Class = c

blackHole(weakVar, unownedVar, unmanagedVar)

// DEMANGLE: $s17reference_storage5ClassCSgXwD
// DEMANGLE: $s17reference_storage5ClassCXoD
// DEMANGLE: $s17reference_storage5ClassCXuD

// CHECK: @sil_weak Optional<Class>
// CHECK: @sil_unowned Class
// CHECK: @sil_unmanaged Class