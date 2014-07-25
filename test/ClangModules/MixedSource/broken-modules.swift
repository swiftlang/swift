// RUN: rm -rf %t && mkdir -p %t
// RUN: not %swift -parse %s -I %S/Inputs/broken-modules/ -module-cache-path %t -enable-source-import -show-diagnostics-after-fatal 2>&1 | FileCheck %s
// RUN: not %swift -parse %s -import-objc-header %t/fake.h -module-cache-path %t 2>&1 | FileCheck -check-prefix=MISSING-HEADER %s
// RUN: not %swift -parse %s -import-objc-header %S/../../Inputs/empty.swift -module-cache-path %t 2>&1 | FileCheck -check-prefix=EMPTY-HEADER %s

// MISSING-HEADER: error: bridging header '{{.*}}/fake.h' does not exist
// MISSING-HEADER-NOT: error

// EMPTY-HEADER-NOT: header

import Nonexistent
// CHECK-NOT: not found
// CHECK: broken-modules.swift:[[@LINE-2]]:8: error: no such module 'Nonexistent'

import MissingDependencyFromSwift
// CHECK-NOT: not found
// CHECK: MissingDependencyFromSwift.swift:1:8: error: no such module 'Dependency'
// CHECK-NOT: no such module 'MissingDependencyFromSwift'

import MissingDependencyFromClang
// CHECK: {{.+}}/Inputs/broken-modules/MissingDependencyFromClang.h:1:9: error: module 'Dependency' not found
// CHECK: error: could not build Objective-C module 'MissingDependencyFromClang'
// CHECK-NOT: no such module 'MissingDependencyFromClang'

import BrokenClangModule
// CHECK: {{.+}}/Inputs/broken-modules/BrokenClangModule.h:2:13: error: redefinition of 'conflict' as different kind of symbol
// CHECK: {{.+}}/Inputs/broken-modules/BrokenClangModule.h:1:5: note: previous definition is here
// CHECK: a-fake-file.h:43:13: error: redefinition of 'conflict2' as different kind of symbol
// CHECK: a-fake-file.h:42:5: note: previous definition is here
// CHECK: error: could not build Objective-C module 'BrokenClangModule'
// CHECK-NOT: no such module 'BrokenClangModule'


let _ = BrokenClangModule.x
// CHECK: broken-modules.swift:[[@LINE-1]]:9: error: module 'BrokenClangModule' has no member named 'x'
