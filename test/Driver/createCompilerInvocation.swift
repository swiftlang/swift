// RUN: %swift-ide-test_plain -test-createCompilerInvocation %s 2>&1
// RUN: %swift-ide-test_plain -test-createCompilerInvocation -emit-executable %s %S/Input/main.swift %S/Input/lib.swift -module-name createCompilerInvocation -emit-module -emit-objc-header -o %t.out 2>&1
// RUN: %swift-ide-test_plain -test-createCompilerInvocation -c %s %S/Input/main.swift %S/Input/lib.swift -module-name createCompilerInvocation -emit-module -emit-objc-header 2>&1
// RUN: not %swift-ide-test_plain -test-createCompilerInvocation -typecheck %s -emit-module-path %t.swiftmodule 2>&1 | %FileCheck --check-prefix=CHECK-FAIL %s
// RUN: not %swift-ide-test_plain -test-createCompilerInvocation -v 2>&1 | %FileCheck --check-prefix=CHECK-FAIL %s
// RUN: %swift-ide-test_plain -test-createCompilerInvocation %s -enable-batch-mode 2>&1 | %FileCheck -allow-empty -check-prefix=CHECK-NOWARN %s

// CHECK-FAIL: error: unable to create a CompilerInvocation
// CHECK-NOWARN-NOT: warning

// RUN: %swift-ide-test_plain -test-createCompilerInvocation \
// RUN:   -module-name foo -emit-module -emit-module-path %t/foo.swiftmodule -emit-objc-header -emit-objc-header-path %t/foo.h -enable-library-evolution -emit-module-interface -emit-module-interface-path %t/foo.swiftinterface -emit-library -emit-tbd -emit-tbd-path %t/foo.tbd -emit-dependencies -serialize-diagnostics %s \
// RUN:   2>&1 | %FileCheck %s --check-prefix=NORMAL_ARGS --implicit-check-not="error: "
// NORMAL_ARGS: Frontend Arguments BEGIN
// NORMAL_ARGS-DAG: -o{{$}}
// NORMAL_ARGS-DAG: foo-{{[a-z0-9]+}}.o
// NORMAL_ARGS-DAG: -c{{$}}
// NORMAL_ARGS-DAG: -module-name
// NORMAL_ARGS-DAG: -emit-module-path
// NORMAL_ARGS-DAG: -emit-module-doc-path
// NORMAL_ARGS-DAG: -emit-module-source-info-path
// NORMAL_ARGS-DAG: -emit-module-interface-path
// NORMAL_ARGS-DAG: -emit-objc-header-path
// NORMAL_ARGS-DAG: -emit-tbd-path
// NORMAL_ARGS-DAG: -serialize-diagnostics-path
// NORMAL_ARGS: Frontend Arguments END

// RUN: %swift-ide-test_plain -test-createCompilerInvocation -force-no-outputs \
// RUN:   -module-name foo -emit-module -emit-module-path %t/foo.swiftmodule -emit-objc-header -emit-objc-header-path %t/foo.h -enable-library-evolution -emit-module-interface -emit-module-interface-path %t/foo.swiftinterface -emit-library -emit-tbd -emit-tbd-path %t/foo.tbd -emit-dependencies -serialize-diagnostics %s \
// RUN:   2>&1 > %t.nooutput_args
// RUN: %FileCheck %s --check-prefix=NOOUTPUT_ARGS --implicit-check-not="error: " < %t.nooutput_args
// RUN: %FileCheck %s --check-prefix=NOOUTPUT_ARGS_NEG --implicit-check-not="error: " < %t.nooutput_args
// NOOUTPUT_ARGS_NEG-NOT: -o{{$}}
// NOOUTPUT_ARGS_NEG-NOT: foo-{{[a-z0-9]+}}.o
// NOOUTPUT_ARGS_NEG-NOT: -o{{$}}
// NOOUTPUT_ARGS_NEG-NOT: -emit
// NOOUTPUT_ARGS_NEG-NOT: -serialize-diagnostics

// NOOUTPUT_ARGS: Frontend Arguments BEGIN
// NOOUTPUT_ARGS-DAG: -typecheck
// NOOUTPUT_ARGS-DAG: -module-name
// NOOUTPUT_ARGS: Frontend Arguments END
