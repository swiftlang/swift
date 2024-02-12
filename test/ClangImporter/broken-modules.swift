// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules/ -enable-objc-interop -show-diagnostics-after-fatal -D MISSING_FROM_MODULE -o /dev/null 2>&1 | %FileCheck -check-prefix CHECK-MODULE-MAP %s
// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules/ -enable-objc-interop -show-diagnostics-after-fatal -o /dev/null 2>&1 | %FileCheck -check-prefix CHECK -check-prefix CHECK-DIRECT %s
// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules/ -enable-objc-interop -show-diagnostics-after-fatal -D INDIRECT -o /dev/null 2>&1 | %FileCheck -check-prefix CHECK -check-prefix CHECK-INDIRECT %s

// FIXME: not every test here depends on Objective-C syntax, this test can be split.

#if MISSING_FROM_MODULE
import MissingHeader
// CHECK-MODULE-MAP: {{.*}}{{/|\\}}Inputs{{/|\\}}custom-modules{{/|\\}}module.modulemap:{{[0-9]+:[0-9]+}}: error: header 'this-header-does-not-exist.h' not found
// CHECK-MODULE-MAP: broken-modules.swift:[[@LINE-2]]:8: error: could not build Objective-C module 'MissingHeader'

#else

#if INDIRECT
import ImportsMissingHeaderIndirect
#else
import ImportsMissingHeader
#endif

// CHECK-INDIRECT: {{.*}}{{/|\\}}Inputs{{/|\\}}custom-modules{{/|\\}}more-custom-modules{{/|\\}}ImportsMissingHeaderIndirect.h:1:9: note: while building module 'ImportsMissingHeader' imported from {{.*}}{{/|\\}}Inputs{{/|\\}}custom-modules{{/|\\}}more-custom-modules{{/|\\}}ImportsMissingHeaderIndirect.h:1:
// CHECK-INDIRECT-NEXT: @import ImportsMissingHeader;

// CHECK: <module-includes>:1:9: note: in file included from <module-includes>:1:
// CHECK-NEXT: #import "{{.*}}ImportsMissingHeader.h"

// CHECK: {{.*}}{{/|\\}}Inputs{{/|\\}}custom-modules{{/|\\}}ImportsMissingHeader.h:1:9: error: 'this-header-does-not-exist.h' file not found

// CHECK-INDIRECT: <module-includes>:1:9: note: in file included from <module-includes>:1:
// CHECK-INDIRECT-NEXT: #import "{{.*}}ImportsMissingHeaderIndirect.h"

// CHECK-INDIRECT: {{.*}}{{/|\\}}Inputs{{/|\\}}custom-modules{{/|\\}}more-custom-modules{{/|\\}}ImportsMissingHeaderIndirect.h:1:9: error: could not build module 'ImportsMissingHeader'
// CHECK-INDIRECT-NEXT: @import ImportsMissingHeader;


// CHECK-DIRECT: broken-modules.swift:{{[0-9]+}}:8: error: could not build Objective-C module 'ImportsMissingHeader'
// CHECK-INDIRECT: broken-modules.swift:{{[0-9]+}}:8: error: could not build Objective-C module 'ImportsMissingHeaderIndirect'

#endif

