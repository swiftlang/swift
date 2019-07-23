// RUN: %target-swift-frontend -typecheck %s -Xcc -Rmodule-import -I %S/Inputs/custom-modules 2>&1 | %FileCheck %s

import Requires.Swift
// CHECK-NOT: error
// CHECK: remark: importing module 'SwiftShims'
// CHECK: remark: importing module 'Requires'
