//// Report dependency cycles involving missing clang modules without crashing
//// rdar://problem/57364033

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/custom-modules %t/

// RUN: %target-swift-frontend -emit-module -DLIB_A %s -module-name A -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t -I %t/custom-modules
// RUN: %target-swift-frontend -emit-module -DLIB_C %s -module-name C -emit-module-path %t/C.swiftmodule -I %t

//// Delete the clang module
// RUN: rm -r %t/custom-modules/

// RUN: not %target-swift-frontend -emit-module -DLIB_D %s -module-name A -emit-module-path %t/D.swiftmodule -I %t 2>&1 | %FileCheck %s

#if LIB_A

#elseif LIB_B

import IndirectImport // From custom-modules
import A

#elseif LIB_C

import B

#elseif LIB_D

import C
// CHECK: <unknown>:0: error: circular dependency between modules 'A' and 'B'

#endif
