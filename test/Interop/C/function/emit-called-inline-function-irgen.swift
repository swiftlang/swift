// Test that we emit LLVM IR for inline functions that are called directly or
// transitively from Swift.
//
// Test that we don't emit LLVM IR for inline functions that are not called from
// Swift.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -I %S/Inputs -Xcc -std=c99 -emit-ir -o - | %FileCheck %s -check-prefix C99 --implicit-check-not notCalled
// RUN: %target-swift-frontend %s -I %S/Inputs -enable-cxx-interop -emit-ir -o - | %FileCheck %s -check-prefix CXX --implicit-check-not notCalled

import EmitCalledFunction

// C99-DAG: define internal i32 @calledFromSwift() #{{[0-9]+}} {
// C99-DAG: define internal i32 @calledTransitively() #{{[0-9]+}} {

// CXX-DAG: define linkonce_odr i32 @_Z15calledFromSwiftv() #{{[0-9]+}} comdat {
// CXX-DAG: define linkonce_odr i32 @_Z18calledTransitivelyv() #{{[0-9]+}} comdat {

calledFromSwift()
