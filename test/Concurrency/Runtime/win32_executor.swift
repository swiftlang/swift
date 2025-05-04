// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library -parse-stdlib -Xfrontend -disable-availability-checking %s %S/Inputs/ExecutorFixture.swift -o %t/win32_executor
// RUN: %target-codesign %t/win32_executor
// RUN: %target-run %t/win32_executor | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=windows-msvc

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import Swift
import _Concurrency

@main
struct Main {
  public static func main() async {
    _ = await ExecutorFixture.test(executor: Win32EventLoopExecutor())

    // Make sure we implement the expected protocols

    // CHECK: Testing Swift.Win32EventLoopExecutor

    // CHECK: o RunLoopExecutor
    // CHECK: o SerialExecutor
    // CHECK: o SchedulableExecutor

    // And make sure that all the tests passed

    // CHECK: ** ALL TESTS PASSED

    _ = await ExecutorFixture.test(executor: Win32ThreadPoolExecutor())

    // CHECK: Testing Swift.Win32ThreadPoolExecutor

    // Make sure we implement the expected protocols

    // CHECK: o SchedulableExecutor
    // CHECK: o TaskExecutor

    // And make sure that all the tests passed

    // CHECK: ** ALL TESTS PASSED
  }
}
