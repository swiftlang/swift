// RUN: %target-swiftxx-frontend -I %S/Inputs %s -emit-ir | %FileCheck %s

import StructuredBindingsGetMethod

let x = testDestructure(20)

// Make sure the definition of `std::get` is emitted.
// CHECK: define {{.*}} @{{_ZSt3getILi0EiiENSt13tuple_elementIXT_ESt4pearIT0_T1_EE4typeERKS4_|"\?\?\$get@\$0A@HH@std@@YAHAEBU\?\$pear@HH@0@@Z"}}
