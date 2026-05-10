// RUN: %empty-directory(%t/cache)
// RUN: %target-swift-frontend %s -swift-version 5 -I %S/Inputs -typecheck -module-cache-path %t/cache -enable-experimental-cxx-interop 2>&1 | %FileCheck --allow-empty %s

// REQUIRES: objc_interop

import CenumsNSOptionsExternC

// CHECK-NOT: warning: imported declaration '' could not be mapped to 'UIPrinter.JobTypes'
// CHECK-NOT: note: please report this issue to the owners of 'CenumsNSOptions'

