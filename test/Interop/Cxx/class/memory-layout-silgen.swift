// RUN: %target-swiftxx-frontend -I %S/Inputs -emit-ir -o - %s | %FileCheck %s

import MemoryLayout

var v = PrivateMemberLayout()
// Check that even though the private member variable `a` is not imported, it is
// still taken into account for computing the layout of the class. In other
// words, we use Clang's, not Swift's view of the class to compute the memory
// layout.
// The important point here is that the second index is 1, not 0.
// CHECK: store i32 42, ptr getelementptr inbounds{{.*}} (i8, ptr @"$s4main1vSo19PrivateMemberLayoutVvp", i{{32|64}} 4), align 4
v.b = 42
