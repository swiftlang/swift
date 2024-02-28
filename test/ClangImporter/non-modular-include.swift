// RUN: %empty-directory(%t)

// Enable -Werror=non-modular-include-in-framework-module
// RUN: not %target-swift-frontend -enable-objc-interop -typecheck %/s -I %S/Inputs/non-modular -F %S/Inputs/non-modular -Xcc -Werror=non-modular-include-in-framework-module -diagnostic-style llvm 2>&1 | %FileCheck --check-prefix=CHECK -check-prefix CHECK-objc %s
// RUN: not %target-swift-frontend -disable-objc-interop -typecheck -Xcc -Werror=non-modular-include-in-framework-module %/s -I %S/Inputs/non-modular -F %S/Inputs/non-modular -diagnostic-style llvm 2>&1 | %FileCheck --check-prefix=CHECK -check-prefix CHECK-native %s

// CHECK: {{.+}}{{/|\\}}non-modular{{/|\\}}Foo.framework{{/|\\}}Headers{{/|\\}}Foo.h:1:10: error: include of non-modular header inside framework module 'Foo'
// CHECK-objc: error: could not build Objective-C module 'Foo'
// CHECK-native: error: could not build C module 'Foo'
// CHECK-NOT: error

// RUN: %target-swift-frontend -debugger-support -typecheck %s -I %S/Inputs/non-modular -F %S/Inputs/non-modular -diagnostic-style llvm 2>&1 | %FileCheck --allow-empty --check-prefix=CHECK-DEBUGGER %s
// RUN: %target-swift-frontend -typecheck %s -I %S/Inputs/non-modular -F %S/Inputs/non-modular -diagnostic-style llvm 2>&1 | %FileCheck --allow-empty --check-prefix=CHECK-DEBUGGER %s

// CHECK-DEBUGGER-NOT: error:


import Foo

_ = Foo.x
