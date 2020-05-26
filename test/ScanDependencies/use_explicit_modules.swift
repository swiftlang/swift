// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache

// RUN: not %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface %s -module-name AccessFilter -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-swift-modules >%t/err.txt 2>&1

// RUN: %FileCheck %s -check-prefix=CHECK-ERR-SWIFT <%t/err.txt

// CHECK-ERR-SWIFT: {{error: no such module 'E'}}

// RUN: not %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface %s -module-name AccessFilter -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-pcms >%t/err.txt 2>&1

// RUN: %FileCheck %s -check-prefix=CHECK-ERR-CLANG <%t/err.txt

// CHECK-ERR-CLANG: {{module 'SwiftShims' is needed but has not been provided, and implicit use of module files is disabled}}

import C
import E
import G
