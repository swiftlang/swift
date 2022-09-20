// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import InvalidStaticMemberVar

func test(i: X) { }

// CHECK-NOT: error
