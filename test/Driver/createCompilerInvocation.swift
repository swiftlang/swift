// RUN: %swift-ide-test_plain -test-createCompilerInvocation %s 2>&1
// RUN: %swift-ide-test_plain -test-createCompilerInvocation -emit-executable %s %S/Input/main.swift %S/Input/lib.swift -module-name createCompilerInvocation -emit-module -emit-objc-header -o %t.out 2>&1
// RUN: %swift-ide-test_plain -test-createCompilerInvocation -c %s %S/Input/main.swift %S/Input/lib.swift -module-name createCompilerInvocation -emit-module -emit-objc-header 2>&1
// RUN: not %swift-ide-test_plain -test-createCompilerInvocation -typecheck %s -emit-module-path %t.swiftmodule 2>&1 | %FileCheck --check-prefix=CHECK-FAIL %s
// RUN: not %swift-ide-test_plain -test-createCompilerInvocation -v 2>&1 | %FileCheck --check-prefix=CHECK-FAIL %s

// CHECK-FAIL: error: unable to create a CompilerInvocation
