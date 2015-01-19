// RUN: not %target-swift-frontend %s -parse -I %S/Inputs -show-diagnostics-after-fatal 2>&1 | FileCheck %s

// CHECK: :[[@LINE+1]]:8: error: module file was created by a newer version of the compiler: {{.*}}too_new.swiftmodule{{$}}
import too_new

// Compiler thinks that the module is empty.
// CHECK: :[[@LINE+1]]:1: error: module 'too_new' has no member named 'foo'
too_new.foo()
