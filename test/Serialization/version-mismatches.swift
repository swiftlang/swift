// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-old/ -show-diagnostics-after-fatal 2>&1 | %FileCheck -check-prefix CHECK -check-prefix TOO-OLD %s
// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-new/ -show-diagnostics-after-fatal 2>&1 | %FileCheck -check-prefix CHECK -check-prefix TOO-NEW %s
// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-old-language/ -show-diagnostics-after-fatal 2>&1 | %FileCheck -check-prefix CHECK -check-prefix LANGUAGE %s
// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-new-language/ -show-diagnostics-after-fatal 2>&1 | %FileCheck -check-prefix CHECK -check-prefix LANGUAGE %s

import Library
// TOO-OLD: :[[@LINE-1]]:8: error: module file was created by an older version of the compiler; rebuild 'Library' and try again: {{.*}}too-old/Library.swiftmodule{{$}}
// TOO-NEW: :[[@LINE-2]]:8: error: module file was created by a newer version of the compiler: {{.*}}too-new/Library.swiftmodule{{$}}
// LANGUAGE: :[[@LINE-3]]:8: error: module compiled with Swift X.Y cannot be imported in Swift {{.+}}.{{.+}}: {{.*}}too-{{old|new}}-language/Library.swiftmodule{{$}}

// Compiler thinks that the module is empty in all cases.
// CHECK: :[[@LINE+1]]:1: error: module 'Library' has no member named 'foo'
Library.foo()
