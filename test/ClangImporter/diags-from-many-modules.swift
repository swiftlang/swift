// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules -module-cache-path %t 2> %t.err.txt
// RUN: %FileCheck -input-file=%t.err.txt %s

import Warnings1
import Warnings2
import Warnings3
import Warnings4
import Warnings5
import Warnings6
import Warnings7
import Warnings8
import Warnings9

// CHECK: Warnings1.h:{{.+}}: warning: 'old' is deprecated
// CHECK: Warnings2.h:{{.+}}: warning: 'old' is deprecated
// CHECK: Warnings3.h:{{.+}}: warning: 'old' is deprecated
// CHECK: Warnings4.h:{{.+}}: warning: 'old' is deprecated
// CHECK: Warnings5.h:{{.+}}: warning: 'old' is deprecated
// CHECK: Warnings6.h:{{.+}}: warning: 'old' is deprecated
// CHECK: Warnings7.h:{{.+}}: warning: 'old' is deprecated
// CHECK: Warnings8.h:{{.+}}: warning: 'old' is deprecated
// CHECK: Warnings9.h:{{.+}}: warning: 'old' is deprecated
