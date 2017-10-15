// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-old/ -show-diagnostics-after-fatal 2>&1 | %FileCheck -check-prefix CHECK -check-prefix TOO-OLD %s
// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-new/ -show-diagnostics-after-fatal 2>&1 | %FileCheck -check-prefix CHECK -check-prefix TOO-NEW %s

// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-old-language/ -show-diagnostics-after-fatal 2>&1 | %FileCheck -check-prefix CHECK -check-prefix LANGUAGE %s
// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-new-language/ -show-diagnostics-after-fatal 2>&1 | %FileCheck -check-prefix CHECK -check-prefix LANGUAGE %s

// Update this line when "-swift-version 3" is no longer supported.
// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-new-language/ -show-diagnostics-after-fatal -swift-version 3 2>&1 | %FileCheck -check-prefix CHECK -check-prefix LANGUAGE-3 %s
// Update this line when "-swift-version 4" is no longer supported.
// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs/too-new-language/ -show-diagnostics-after-fatal -swift-version 4 2>&1 | %FileCheck -check-prefix CHECK -check-prefix LANGUAGE-4 %s

import Library
// TOO-OLD: :[[@LINE-1]]:8: error: module file was created by an older version of the compiler; rebuild 'Library' and try again: {{.*}}too-old/Library.swiftmodule{{$}}
// TOO-NEW: :[[@LINE-2]]:8: error: module file was created by a newer version of the compiler: {{.*}}too-new/Library.swiftmodule{{$}}

// Update this line when the default language version changes
// LANGUAGE: :[[@LINE-5]]:8: error: module compiled with Swift X.Y cannot be imported in Swift {{.+}}.{{.+}}: {{.*}}too-{{old|new}}-language/Library.swiftmodule{{$}}

// Update this line when "-swift-version 3" is no longer supported.
// LANGUAGE-3: :[[@LINE-8]]:8: error: module compiled with Swift X.Y cannot be imported in Swift 3.{{.+}}: {{.*}}too-{{old|new}}-language/Library.swiftmodule{{$}}
// Update this line when "-swift-version 3" is no longer supported.
// LANGUAGE-4: :[[@LINE-10]]:8: error: module compiled with Swift X.Y cannot be imported in Swift 4.{{.+}}: {{.*}}too-{{old|new}}-language/Library.swiftmodule{{$}}

// Compiler thinks that the module is empty in all cases.
// CHECK: :[[@LINE+1]]:1: error: module 'Library' has no member named 'foo'
Library.foo()
