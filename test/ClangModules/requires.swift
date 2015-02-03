// RUN: not %target-swift-frontend -parse %s -I %S/Inputs/custom-modules 2>&1 | FileCheck %s

import Requires.Swift // OK
import Requires.NotSwift
// CHECK-NOT: error
// CHECK: error: no such module 'Requires.NotSwift'
// CHECK-NOT: error
