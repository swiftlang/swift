// RUN: %sil-opt %s -inout-deshadow | FileCheck %s

sil_stage canonical

import Builtin
import swift

class C {
}

struct NontrivialStruct {
  var a: Int
  var b: C
  func foo()
}


sil @takeNontrivial : $[cc(method), thin] ((), [inout] NontrivialStruct) -> ()

// CHECK-LABEL: sil @NontrivialTest
sil @NontrivialTest : $[thin] (a: [inout] NontrivialStruct) -> () {
bb0(%0 : $*NontrivialStruct):
  %1 = alloc_stack $NontrivialStruct  // var a    // users: %20, %18, %7, %9, %2, %4
  %2 = struct_element_addr %1#1 : $*NontrivialStruct, #a // user: %3
  %3 = struct_element_addr %2 : $*Int64, #value   // user: %10
  %4 = struct_element_addr %1#1 : $*NontrivialStruct, #b // user: %12
  %5 = load %0 : $*NontrivialStruct               // user: %6
  %6 = copy_value %5 : $NontrivialStruct          // user: %7
  store %6 to %1#1 : $*NontrivialStruct
  %8 = function_ref @takeNontrivial : $[cc(method), thin] ((), [inout] NontrivialStruct) -> () // user: %9
  %9 = apply %8(%1#1) : $[cc(method), thin] ((), [inout] NontrivialStruct) -> ()
  %10 = load %3 : $*Builtin.Int64                 // user: %11
  %11 = struct $Int64 (%10 : $Builtin.Int64)      // user: %13
  %12 = load %4 : $*C                             // user: %13
  %13 = struct $NontrivialStruct (%11 : $Int64, %12 : $C) // user: %14
  %14 = copy_value %13 : $NontrivialStruct        // user: %16
  %15 = load %0 : $*NontrivialStruct              // user: %17
  store %14 to %0 : $*NontrivialStruct
  destroy_value %15 : $NontrivialStruct
  %18 = load %1#1 : $*NontrivialStruct            // user: %19
  destroy_value %18 : $NontrivialStruct
  dealloc_stack %1#0 : $*@local_storage NontrivialStruct
  %21 = tuple ()                                  // user: %22
  return %21 : $()
}
