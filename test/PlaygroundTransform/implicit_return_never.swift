// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -disable-playground-transform -o %t/main %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %{python} %S/../Inputs/not.py "%target-run %t/main --crash" 2>&1 | %FileCheck -check-prefix=CRASH-CHECK %s
// REQUIRES: executable_test

// The runtime error format changed after the 5.3 release.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// NOTE: not.py is used above instead of "not --crash" because simctl's exit
// status doesn't reflect whether its child process crashed or not. So "not
// --crash %target-run ..." always fails when testing for the iOS Simulator.
// The more precise solution would be to use a version of 'not' cross-compiled
// for the simulator.

func f() -> Int {
    fatalError()
// CRASH-CHECK: {{.*}}/main.swift:[[@LINE-1]]: Fatal error
}

f()
