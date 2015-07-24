// RUN: not %target-swift-frontend %s -parse -I %S/Inputs -show-diagnostics-after-fatal 2>&1 | FileCheck %s

// CHECK: :[[@LINE+1]]:8: error: module file was created by an older version of the compiler; rebuild 'too_old' and try again: {{.*}}too_old.swiftmodule{{$}}
import too_old

// Compiler thinks that the module is empty.
// CHECK: :[[@LINE+1]]:1: error: module 'too_old' has no member named 'foo'
too_old.foo()
