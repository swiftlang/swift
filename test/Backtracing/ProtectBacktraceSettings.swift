// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/ProtectBacktraceSettings
// RUN: %target-codesign %t/ProtectBacktraceSettings
// RUN: env %env-SWIFT_BACKTRACE=enable=yes %target-run %t/ProtectBacktraceSettings > %t/output 2>&1
// RUN: %FileCheck %s --implicit-check-not="swift runtime:" < %t/output
// RUN: env %env-SWIFT_BACKTRACE=enable=yes,output-to=%t/backtrace.log %target-run %t/ProtectBacktraceSettings > %t/output 2>&1
// RUN: %FileCheck %s --implicit-check-not="swift runtime:" < %t/output

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=linux-gnu

// CHECK: Backtrace settings initialized
print("Backtrace settings initialized")
