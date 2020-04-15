// Test that Swift emits code for a C++ inline function that is called from
// another C++ inline function, but not directly from Swift code.
//
// We should not, however, emit code for functions that aren't called even
// indirectly from Swift.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -I %S/Inputs -enable-cxx-interop -emit-ir -o - | %FileCheck %s

import EmitCalledFunction

// Unfortunately, we have to repeat the CHECK-NOT to express that notCalled()
// should not occur anywhere in the output.

// CHECK-NOT: @_Z9notCalledv
// CHECK-DAG: define linkonce_odr i32 @_Z14calledDirectlyv() #{{[0-9]+}} comdat {
// CHECK-NOT: @_Z9notCalledv
// CHECK-DAG: define linkonce_odr i32 @_Z16calledIndirectlyv() #{{[0-9]+}} comdat {
// CHECK-NOT: @_Z9notCalledv
calledDirectly()
