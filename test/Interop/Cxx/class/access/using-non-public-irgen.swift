// RUN: %target-swift-emit-ir -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

// A method redeclared by a using-declaration in an intermediate class
// (PrivUser) should be forwarded to directly from the derived class
// (PublPrivUser), rather than through the intermediate class's forwarding
// method.

import UsingNonPublic

public func callPublPrivUser() -> Return {
  return PublPrivUser().publUsingProt()
}

// CHECK-SYSV: define {{.*}} @_ZNK12PublPrivUser{{[0-9]+}}__synthesizedBaseCall_publUsingProt
// CHECK-WIN: define {{.*}} @"?__synthesizedBaseCall_publUsingProt{{.*}}@PublPrivUser@@
// CHECK-NOT: define
// CHECK-SYSV: call {{.*}} @_ZNK4Base13publUsingProtEv
// CHECK-WIN: call {{.*}} @"?publUsingProt@Base@@
