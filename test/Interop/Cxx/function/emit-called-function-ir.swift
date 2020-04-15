// This is a reproducer for a bug where Swift was not emitting code for a
// C++ inline function called from another inline function.
//
// In our example, inline function foo() calls inline function bar(). The bug
// resulted in bar() not being emitted.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -I %S/Inputs -enable-cxx-interop -emit-ir -o - | %FileCheck %s

import EmitCalledFunction

// CHECK-DAG: define linkonce_odr i32 @_Z3foov() #{{[0-9]+}} comdat {
// CHECK-DAG: define linkonce_odr i32 @_Z3barv() #{{[0-9]+}} comdat {
foo()
